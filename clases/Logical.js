//import Expression from './Expression';
const Type = require('./Type');
const Value = require('./Value');

class Logical {
    constructor(left, right, dataType, structureType, _row, _column) {
        this.dataType = dataType;
        this.node_left = left;
        this.node_right = right;
        this.structureType = structureType;
        this.row = _row;
        this.column = _column;
    }

    /**
     * 
     * @param {SymbolTable} tab tabla de simbolos del ambito actual
     * @param {Counters} count 
     * @returns null
     */

    operate(tab, count) {
        /**
         * Primero se deben verificar que los atributos sean validados
         */
        let tempL = null;
        let tempR = null;

        if (this.node_left != null) {
            tempL = this.node_left.operate(tab, count);
        }

        if (this.node_right != null) {
            tempR = this.node_right.operate(tab, count);
        }

        if (tempL != null && tempR != null) {
            /**
             * Si los atributos son validos entonces se ejecuta la logica de la instruccion
             * en este caso se realizaran las siguientes operaciones:
             * - AND
             * - OR
             * - NOT
             * Para las diferentes combinaciones de tipos de datos
             */
            if (tempL.structureType === Type.VALOR && tempR.structureType === Type.VALOR) {
                /**
                 * Combinaciones de los Operando izquierdo y derecho
                 */
                if (tempL.dataType === Type.BOOL && tempR.dataType === Type.BOOL) {
                    if (null != this.dataType) {
                        /**
                         * Añadir o quitar else if segun los requeriemientos del lenguaje a interpretear
                         */
                        switch (this.dataType) {
                            case Type.AND:
                                return new Value(tempL.value && tempR.value, Type.BOOL, Type.VALOR, this.row, this.column);
                            case Type.OR:
                                return new Value(tempL.value || tempR.value, Type.BOOL, Type.VALOR, this.row, this.column);
                            default:
                                break;
                        }
                    }
                    count.putOperatorError(this.dataType, this.row, this.column);;
                }
            }
        } else if (tempR === null && tempL != null) {
            if (tempL.structureType === Type.VALOR) {
                if (this.dataType === Type.NOT) {
                    /**
                     * Añadir o quitar else if segun los requeriemientos del lenguaje a interpretear
                     */
                    if (tempL.value === true) {
                        return new Value(false, Type.BOOL, Type.VALOR, this.row, this.column);
                    } else {
                        return new Value(true, Type.BOOL, Type.VALOR, this.row, this.column);
                    }
                }
            }
            count.putOperatorError(this.dataType, this.row, this.column);;
        }
        return null;
    }

}

module.exports = Logical;