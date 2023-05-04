//import Expression from './Expression';
const Symbol = require('./Symbol');
const Value = require('./Value');
const Type = require('./Type');

class Declaration {
    constructor(_id, _value, _dataType, _structureType, _scopeType,  _row, _column) {
        this.id = _id;
        this.value = _value;
        this.dataType = _dataType;
        this.structureType = _structureType;
        this.scopeType = _scopeType;
        this.row = _row;
        this.column = _column;
    }

    /**
     * 
     * @param {SymbolTable} tab tabla de simbolos del ambito actual
     * @param {Counters} count Objeto para sustituir variables globales
     * @returns null
     * 
     * La instruccion declaracion consta de la siguinete estructura
     * <Tipo de dato> <identificador> [ = <expresion> ]?;
     */

    operate(tab, count) {
        
        let tmpExp = null;

        if (this.value != null) {
            /**
             * Se evalua que la expresion a almacenar en la variable sea valida
             */
            tmpExp = this.value.operate(tab, count);

            if (tmpExp == null) {
                /**
                 * Si la expresion no es valida se añade un nuevo error
                 */
                count.putError(Type.SEMANTICO, "Hubo un error al realizar la declaracion de la variable " + this.id + ".", this.row, this.column);
                return null;
            }
            /**
             * Se tiene que verificar que el tipo de dato de la expresion
             * sea el mismo que la variable
             */
            if (this.checkType(tmpExp, count)) {
                /**
                 * Se retorna null solo si los tipos no coinciden
                 */
                return null;
            }
        }
        /**
         * Debido a que se maneja una lista simplemente enlazada para la tabla de simbolo
         * en cada ambito, es necesario el solo verificar que la variable existe unicamente en
         * el ambito actual y evitar conflictos con ambitos superiores como el ambito global
         */
        let a = tab.existsDirect(this.id);
        
        if (tmpExp == null) {
            switch (this.dataType) {
                /**
                 * Si la declaracion en el texto de entrada es por ejemplo Integer a;
                 * entonces se configura el valor por defecto de cada tipo de dato
                 */
                case Type.DECIMAL:
                    tmpExp = new Value(0.0, this.dataType, this.structureType, this.row, this.column);
                    break;
                case Type.ENTERO:
                    tmpExp = new Value(0, this.dataType, this.structureType, this.row, this.column);
                    break;
                case Type.CHAR:
                    tmpExp = new Value('\0', this.dataType, this.structureType, this.row, this.column);
                    break;
                case Type.BOOL:
                    tmpExp = new Value(false, this.dataType, this.structureType, this.row, this.column);
                    break;
                default:
                    break;
            }
        }
        if (a === false) {
            /**
             * Si la variable aun no ha sido declarado en el ambito actual entonces se procede a crear
             * el simbolo nuevo he insertarlo en la tabla de simbolos
             */
            tab.addSymbolDirect(new Symbol(this.id, tmpExp.dataType, tmpExp.structureType, this.scopeType, tmpExp.value));
            /**
             * putSymbol sirve para almacenar los simbolos para el reporte final
             */
            count.putSymbol(this.id, tmpExp.dataType, tmpExp.structureType, this.scopeType, tmpExp.value);
            return true;
        } else {
            /**
             * Si la variable ya existe entonces es error, se crea un nuevo error
             */
            count.putError(Type.SEMANTICO, "La variable " + this.id + " ya fue declarada en este ambito.", this.row, this.column);
        }
        return null;
    }

    checkType(val, count) {
        
        if (this.dataType !== val.dataType) {
            /**
             * Si los tipos de datos no coinciden entonces se añade un nuevo error
             */
            count.putError(Type.SEMANTICO, 'Tipo de la EXPRESION ' + val.dataType + ' No Asignable a un ' + this.dataType + '.', this.row, this.column);
            return true;
        }
        return false;
    }
}

module.exports = Declaration;