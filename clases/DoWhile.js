//import Expression from './Expression';
const Type = require('./Type');
const SymbolTable = require('./SymbolTable');

class DoWhile {
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
     * La instruccion DoWhile consta de la siguinete estructura
     * do { <Conjunto de instrucciones> }  While( <expresion> );
     */
    operate(tab, count) {
        /**
         * Se crea una nueva tabla de simbolo debido a que se usara un nuevo ambito
         * se crea una nueva tabla de simbolos, se esta creando un ambito nuevo y se esta añadiendo un nodo nuevo
         * a las lista de tablas de simbolos
         */
        let s = new SymbolTable(tab);
        let r;
        do {
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
                /**
                 * Si hubo un error en la operacion de la expresion se interrumpe la ejecucion
                 */
                count.putError(Type.SINTACTICO, "No se puede ejecutar la operacion DOWHILE, se necesita una condicion logica o relacional.", this.row, this.column);
                return null;
            }
            if (r.dataType !== Type.BOOL) {
                /**
                 * Si la expresion es valida, se debe verificar que el tipo de dato es BOOL de lo contrario es error
                 */
                count.putError(Type.SINTACTICO, "No se puede ejecutar la operacion " + r.dataType + ", se necesita una condicion logica o relacional.", this.row, this.column);
                return null;
            }
            /**
             * El DoWhile se ejecutara hasta que la expresion de false
             */
        } while (r.value === true)

        return true;
    }
}

module.exports = DoWhile;