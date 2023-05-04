//import Expression from './Expression';
const Type = require('./Type');

class Unary {
    constructor(_id, _dataType, _row, _col){
        this.id = _id;
        this.dataType = _dataType;
        this.row = _row;
        this.column = _col;
    }

    /**
     * 
     * @param {SymbolTable} tab tabla de simbolos del ambito actual
     * @param {Counters} count Objeto para sustituir variables globales
     * @returns null
     * 
     * La instruccion Unary consta de la siguinete estructura
     *  <identificador> ++;
     *  <identificador> --;
     */
    operate(tab, count){
        /**
         * Se busca la variable dentro de la tabla de simbolos
         */
        let a = tab.getSymbol(this.id);
        
        if (a == null) {
            /**
             * Se crea un nuevo error si no se encontro la variable
             */
            count.putError(Type.SEMANTICO, "Variable " + this.id + " no encontrada.", this.row, this.column);
        } else {
            /**
             * Validaciones necesarias para eviatar conflictos en la ejecucion
             */
            if (a.dataType !== Type.ENTERO && a.dataType !== Type.DECIMAL) {
                count.putError(Type.SEMANTICO, 'No se puede asignar VALOR a ' + a.id + ' Tipo incompatible para operador unario.', this.row, this.column);
                return null;
            }
            /**
             * Se ejecuta la instruccion
             */
            if(this.dataType === Type.INCREMENTO){
                a.value = a.value + 1;
                return true;
            }else {
                a.value = a.value - 1;
                return true;
            }
            

        }
        return null;
    }
}


module.exports = Unary;