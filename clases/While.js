//import Expression from './Expression';
const Type = require('./Type');
const SymbolTable = require('./SymbolTable');

class While {
    constructor(e, c, _row, _column) {
        this.row = _row;
        this.column = _column;
        this.exp = e;
        this.body = c;
    }

    /**
     * 
     * @param {SymbolTable} tab tabla de simbolos del ambito actual
     * @param {Counters} count Objeto para sustituir variables globales
     * @returns null
     * 
     * La instruccion While consta de la siguinete estructura
     * While <expresion> { <Conjunto de instrucciones> };
     */
    operate(tab, count) {
        /**
         * Se crea una nueva tabla de simbolo debido a que se usara un nuevo ambito
         * se crea una nueva tabla de simbolos, se esta creando un ambito nuevo y se esta añadiendo un nodo nuevo
         * a las lista de tablas de simbolos
         */
        let s = new SymbolTable(tab);
        /**
         * Se evalua la expresion del While para verificar si el ciclo continua o se interrumpe
         */
        let r = this.exp.operate(tab, count);
        /**
         * Si hubo un error en la operacion de la expresion se interrumpe la ejecucion
         */
        if (r == null) {
            count.putError(Type.SINTACTICO, "No se puede ejecutar la operacion WHILE, se necesita una condicion logica o relacional.", this.row, this.column);
            return null;
        }
        if (r.dataType !== Type.BOOL) {
            count.putError(Type.SINTACTICO, "No se puede ejecutar la operacion " + r.dataType + ", se necesita una condicion logica o relacional.", this.row, this.column);
            return null;
        }
        
        while (r.value === true) {
            /**
             * Se ejecuta el bloque de instrucciones dentro del While
             * con la nueva tabla de simbolos
             */
            for (let i = 0; i < this.body.length; i++) {
                this.body[i].operate(s, count) //<= tabla de simbolos creada para el nuevo ambito
            }

            /**
             * Se evalua la expresion del While para verificar si el ciclo continua o se interrumpe
             */
            r = this.exp.operate(s, count);
            if (r == null) {
                count.putError(Type.SINTACTICO, "No se puede ejecutar la operacion " + r.dataType + ", se necesita una condicion logica o relacional.", this.row, this.column);
                return null;
            }
            if (r.dataType !== Type.BOOL) {
                count.putError(Type.SINTACTICO, "No se puede ejecutar la operacion " + r.dataType + ", se necesita una condicion logica o relacional.", this.row, this.column);
                return null;
            }
        }

        return true;
    }
}

module.exports = While;