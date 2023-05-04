//import Expression from './Expression';
const Type = require('./Type');
const Value = require('./Value')
const Symbol = require('./Symbol')
const SymbolTable = require('./SymbolTable')

class Call {
    constructor(_id, _dataType, _structureType, _param, _row, _column) {
        this.id = _id;
        this.dataType = _dataType;
        if (_param == null) {
            this.param = []
        } else {
            this.param = _param;
        }
        this.structureType = _structureType;
        this.column = _column;
        this.row = _row;

    }

    /**
     * 
     * @param {SymbolTable} tab tabla de simbolos del ambito actual
     * @param {Counters} count Objeto para sustituir variables globales
     * @returns null
     * 
     * La instruccion call o llamada a funcion consta de la siguinete estructura
     * <identificador> ([<expresion>]*);
     */
    operate(tab, count) {

        /**
         * Al igual que los simbolos son almacenados en una tabla
         * Las funciones son almacenadas en una lista para su rapido acceso
         */
        let f = tab.getFunction(this.id);
        if (f == null) {
            /**
             * Si la funcion no existe se marca error
             */
            count.putError(Type.SEMANTICO, "Funcion: " + this.id + ", No Declarada.", this.row, this.column);
            return null;
        }
        /**
         * se crea una nueva tabla de simbolos, se esta creando un ambito nuevo y se esta añadiendo un nodo nuevo
         * a las lista de tablas de simbolos
         */
        let newScope = new SymbolTable(tab, count);
        /**
         * Si la funcion tiene parametros se deben configurar y ser añadidos al nuevo ambito
         */
        if (f.param != null) {
            /**
             * Dedibo a que no se aceptan funciones con el mismo identificador
             * se verifica que la cantidad de parametros en la funcion y la cantidad de 
             * parametros en la llamada a la funcion sea la misma
             */
            if (f.param.length !== this.param.length) {
                /**
                 * Si la cantidad de parametro no coincide entonces se marca error
                 */
                count.putError(Type.SEMANTICO, "La Cantidad de parametros de la LLAMADA no Coinciden con la FUNCION.", this.row, this.column);
                return null;
            }
            for (let i = 0; i < f.param.length; i++) {
                /**
                 * Se recorre la lista de parametros en la FUNCION.
                 * Se busca que cada parametro aun no haya sido declarado en el nuevo ambito
                 */
                if (!newScope.existsDirect(f.param[i].id)) {
                    /**
                     * Se opera la expresion que se encuentra en los parametros de la LLAMADA a la funcion
                     */
                    let tmpV = this.param[i].operate(newScope, count);
                    if (tmpV == null) {
                        /**
                         * Generar error y parar la ejecucion si se encuentra algun error
                         */
                        count.putError(Type.SEMANTICO, "Parametro en la posicion " + (i + 1) + " NO VALIDO.", this.row, this.column);
                        return null;
                    }
                    if (tmpV.dataType !== f.param[i].dataType) {
                        /**
                         * Si el tipo de dato en el parametro de la FUNCION no coincide
                         * con el tipo de dato en el parametro de la LLAMADA a la funcion entonces
                         * se genera un error y se cancela la ejecucion de esta instruccion
                         */
                        count.putError(Type.SEMANTICO, "Tipos incompatibles del parametro en la posicion " + (i + 1), this.row, this.column);
                        return null;
                    }
                    /**
                     * Si todo esta correcto se ingresa el sibolo a la nueva tabla de simbolo que sera el nuevo ambito
                     */
                    newScope.addSymbolDirect(new Symbol(f.param[i].id, tmpV.dataType, tmpV.structureType, Type.LOCAL, tmpV.value));
                }

            }
        }

        /**
         * Se ejecuta el bloque de instrucciones de la FUNCION
         * pero se pasa como parametro de tabla de simbolo
         * la nueva tabla que se creo para simular el nuevo ambito
         */
        for (let i = 0; i < f.body.length; i++) {
            f.body[i].operate(newScope, count);// <= se usa newScope y no tab
        }
        return new Value(null, Type.NULL, Type.VALOR, this.row, this.column);

    }
}

module.exports = Call;