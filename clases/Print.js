//import Expression from './Expression';
const Type = require('./Type');

class Print {
    constructor(val, _dataType, _structureType, _row, _column) {
        this.value = val;
        this.dataType = _dataType;
        this.structureType = _structureType;
        this.row = _row;
        this.column = _column;
    }

    /**
     * 
     * @param {SymbolTable} tab tabla de simbolos del ambito actual
     * @param {Counters} count Objeto para sustituir variables globales
     * @returns null
     * 
     * La instruccion print consta de la siguinete estructura
     * <reservada>(<expresion a imprimir>);
     */

    operate(tab, count) {
        /**
         * Evaluar la expresion, puede ser de cualquier clase sin embargo es seguro
         * que se devolvera un objeto Value()
         */
        let e = this.value.operate(tab, count);
        
        if (e != null) {
            /**
             * Si la expresion es valida se añade la impresion en la variable de cadena de salida
             */
            count.output += e.value + "\n";
        } else {
            /**
             * Si la expresion no es valida se crea un nuevo error
             */
            count.putError(Type.SEMANTICO, "Hubo un error al Ejecutar Imprimir.", this.row, this.column);
        }
        /**
         * Se retorna null ya que de la instruccion print no se espera un valor de retorno
         */
        return null;
    }

}

module.exports = Print;