//import Expression from './Expression';
const Type = require('./Type')

class Value {
    constructor(val, _dataType, _structureType, _row, _column) {
        this.value = val;
        this.dataType = _dataType;
        this.structureType = _structureType;
        this.scopeType = '';
        this.row = _row;
        this.column = _column;
    }

    /**
     * 
     * @param {SymbolTable} tab tabla de simbolos del ambito actual
     * @param {Counters} count Objeto para sustituir variables globales
     * @returns null
     * 
     * Esta clase sirve para limpiar los valores puros o valores primitivos que seran
     * usados en otras operaciones
     */
    operate(tab, count) {
        /**
         * Este if nos sirve para saber que tipo de accion vamos a tomar, no es lo mismo
         * limpiar o preparar un valor primitivo que un objeto o un arreglo
         */
        if (this.structureType === Type.VALOR) {
            switch (this.dataType) {
                /**
                 * Devolvemos el objeto que sera utilizado para las operaciones dentro del interprete
                 */
                case Type.ENTERO:

                    return new Value(Number(this.value), this.dataType, this.structureType, this.row, this.column);

                case Type.DECIMAL:

                    return new Value(Number(this.value), this.dataType, this.structureType, this.row, this.column);

                case Type.DEFAULT:
                    
                    return new Value(Number(this.value), this.dataType, this.structureType, this.row, this.column);

                case Type.CADENA:
                    /**
                     * Se crean sustituyen los caracteres '\' 'n' por el caracter de salto de linea '\n'
                     * asi mismo con \t y \r
                     */
                    this.value = this.value.replace(/\\n/g, '\n');
                    this.value = this.value.replace(/\\t/g, '\t');
                    this.value = this.value.replace(/\\r/g, '\r');
                    if (this.value.toString().startsWith("\"")) {
                        this.value = this.value.toString().substring(1, this.value.toString().length - 1);
                    }
                    this.value = this.value.toString().replace(/\\\"/g, "\"");
                    return new Value(this.value, this.dataType, this.structureType, this.row, this.column);

                case Type.BOOL:

                    if(this.value === true){
                        return new Value(true, this.dataType, this.structureType, this.row, this.column);
                    }
                    return new Value(false, this.tydataTypepe, this.structureType, this.row, this.column);

                case Type.NULL:

                    return new Value('null', Type.NULL, Type.VALOR, this.row, this.column);

                case Type.CARACTER:
                    /**
                     * Si el token que se obtiene del analizador lexico es "'c'" entonces 
                     * se retiran las comillas simples
                     */
                    let ret = this.value.replace(/'/g,'');
                    
                    if(String(ret) === "\\n"){
                        return new Value(10, Type.CARACTER, Type.VALOR, this.row, this.column);
                    } else if(ret === "\\r"){
                        return new Value(8, Type.CARACTER, Type.VALOR, this.row, this.column);
                    } else if(ret === "\\t"){
                        return new Value(9, Type.CARACTER, Type.VALOR, this.row, this.column);
                    }
                    return new Value(ret.charCodeAt(0), Type.CARACTER, Type.VALOR, this.row, this.column);

                case Type.ID:

                    let a = tab.exists(this.value);
                    if (a) {
                        let r = tab.getSymbol(this.value);
                        return new Value(r.value, r.dataType, r.structureType, r.row, r.column);

                    } else {

                        count.putError(Type.SEMANTICO, "Variable " + this.value + " no encontrada.", this.row, this.column);
                        
                        return null;
                    }

                default:

                    count.putError(Type.SEMANTICO, "Tipo " + this.dataType + " no Valido.", this.row, this.column);
                    return new Value(null, Type.ERROR, Type.ERROR, this.row, this.column);
            }
        } else {
            /*
            Procesar otro tipos de datos como vectores o matrices
            */
        }
    }

}

module.exports = Value;