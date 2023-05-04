//import Expression from './Expression';
const Type = require('./Type');
const SymbolTable = require('./SymbolTable');

class For {
    constructor(_decla, _exp, _assig, _body, _row, _col) {
        this.declaration = _decla;
        this.exp = _exp;
        this.assignment = _assig
        this.body = _body;
        this.row = _row;
        this.column = _col;
    }

    /**
     * 
     * @param {SymbolTable} tab tabla de simbolos del ambito actual
     * @param {Counters} count Objeto para sustituir variables globales
     * @returns null
     * 
     * La instruccion For consta de la siguinete estructura
     * For ( <Asignacion o declaracion> ; <expresion>; <Asignacion o incremento o decremento> ) { < Conjunto de instrucciones> };
     */
    operate(tab, count) {
        /**
         * Se crea una nueva tabla de simbolo debido a que se usara un nuevo ambito
         * se crea una nueva tabla de simbolos, se esta creando un ambito nuevo y se esta añadiendo un nodo nuevo
         * a las lista de tablas de simbolos
         */
        let s = new SymbolTable(tab);
        if (this.declaration != null) {
            /**
             * Se debe operar la declaracion y verificar que no de error
             */
            if (this.declaration.operate(s, count) == null) {
                /**
                 * Crear un nuevo error
                 */
                return null;
            }
        }

        /**
         * Se debe operar la expresion ya que sera la bandera para romper el ciclo for
         */
        let r = this.exp.operate(s, count);
        if (r == null) {
            /**
             * Si hubo un error en la operacion de la expresion se interrumpe la ejecucion
             */
            count.putError(Type.SINTACTICO, "No se puede ejecutar la operacion FOR, se necesita una condicion logica o relacional.", this.row, this.column);
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
         * El For se ejecutara hasta que la expresion de false
         */
        while (r.value === true) {
            /**
             * Se ejecuta el bloque de instrucciones dentro del While
             * con la nueva tabla de simbolos
             */
            for (let i = 0; i < this.body.length; i++) {
                this.body[i].operate(s, count); //<= tabla de simbolos creada para el nuevo ambito
            }

            /**
             * Se ejecuta nuevamenta la asignacion si existe error se interrumpe la ejecucion del For
             */
            if (this.assignment != null) {
                if(this.assignment.operate(s, count) == null) {
                    /**
                     * Generar nuevo error
                     */
                    return null;
                }
            }
            /**
             * Se evalua nuevamente la expresion que sera la bandera de salida del ciclo For
             */
            r = this.exp.operate(s, count);
            if (r == null) {
                count.putError(Type.SINTACTICO, "No se puede ejecutar la operacion FOR, se necesita una condicion logica o relacional.", this.row, this.column);
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

module.exports = For;