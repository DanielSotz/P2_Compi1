//import Expression from './Expression';
const Type = require('./Type');
const Value = require('./Value');

class Relational {
    constructor(left, right, _dataType, _structureType, _row, _column) {
        this.dataType = _dataType;
        this.node_left = left;
        this.node_right = right;
        this.structureType = _structureType;
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
        if (this.node_left !== null) {
            tempL = this.node_left.operate(tab, count);
        }

        if (this.node_right !== null) {
            tempR = this.node_right.operate(tab, count);
        }

        if (tempL !== null && tempR !== null) {
            /**
             * Si los atributos son validos entonces se ejecuta la logica de la instruccion
             * en este caso se realizaran las siguientes operaciones:
             * - IGUAL o IDENTICO
             * - DIFERENTE
             * - MAYOR
             * - MENOR
             * - MAYOR QUE
             * - MENOR QUE
             * Para las diferentes combinaciones de tipos de datos
             */
            if (tempL.structureType === Type.VALOR && tempR.structureType === Type.VALOR) {
                /**
                 * Combinaciones de los Operando izquierdo y derecho
                 */
                if (tempL.dataType === Type.ENTERO && tempR.dataType === Type.ENTERO) {
                    /**
                     * Añadir o quitar else if segun los requeriemientos del lenguaje a interpretear
                     */
                    if (this.dataType === Type.IDENTICO) {
                        return new Value(tempL.value === tempR.value, Type.BOOL, Type.VALOR, this.row, this.column);
                    } else if (this.dataType === Type.DIFERENTE) {
                        return new Value(tempL.value !== tempR.value, Type.BOOL, Type.VALOR, this.row, this.column);
                    } else if (this.dataType === Type.MAYOR) {
                        return new Value(tempL.value > tempR.value, Type.BOOL, Type.VALOR, this.row, this.column);
                    } else if (this.dataType === Type.MENOR) {
                        return new Value(tempL.value < tempR.value, Type.BOOL, Type.VALOR, this.row, this.column);
                    } else if (this.dataType === Type.MAYORIGUAL) {
                        return new Value(tempL.value >= tempR.value, Type.BOOL, Type.VALOR, this.row, this.column);
                    } else if (this.dataType === Type.MENORIGUAL) {
                        return new Value(tempL.value <= tempR.value, Type.BOOL, Type.VALOR, this.row, this.column);
                    }
                    return null;
                } else if (tempL.dataType === Type.ENTERO && tempR.dataType === Type.DECIMAL) {
                    /**
                     * Añadir o quitar else if segun los requeriemientos del lenguaje a interpretear
                     */
                    if (this.dataType === Type.IDENTICO) {
                        return new Value(tempL.value === tempR.value, Type.BOOL, Type.VALOR, this.row, this.column);
                    } else if (this.dataType === Type.DIFERENTE) {
                        return new Value(tempL.value !== tempR.value, Type.BOOL, Type.VALOR, this.row, this.column);
                    } else if (this.dataType === Type.MAYOR) {
                        return new Value(tempL.value > tempR.value, Type.BOOL, Type.VALOR, this.row, this.column);
                    } else if (this.dataType === Type.MENOR) {
                        return new Value(tempL.value < tempR.value, Type.BOOL, Type.VALOR, this.row, this.column);
                    } else if (this.dataType === Type.MAYORIGUAL) {
                        return new Value(tempL.value >= tempR.value, Type.BOOL, Type.VALOR, this.row, this.column);
                    } else if (this.dataType === Type.MENORIGUAL) {
                        return new Value(tempL.value <= tempR.value, Type.BOOL, Type.VALOR, this.row, this.column);
                    }
                } else if (tempL.dataType === Type.DECIMAL && tempR.dataType === Type.ENTERO) {
                    /**
                     * Añadir o quitar else if segun los requeriemientos del lenguaje a interpretear
                     */
                    if (this.dataType === Type.IDENTICO) {
                        return new Value(tempL.value === tempR.value, Type.BOOL, Type.VALOR, this.row, this.column);
                    } else if (this.dataType === Type.DIFERENTE) {
                        return new Value(tempL.value !== tempR.value, Type.BOOL, Type.VALOR, this.row, this.column);
                    } else if (this.dataType === Type.MAYOR) {
                        return new Value(tempL.value > tempR.value, Type.BOOL, Type.VALOR, this.row, this.column);
                    } else if (this.dataType === Type.MENOR) {
                        return new Value(tempL.value < tempR.value, Type.BOOL, Type.VALOR, this.row, this.column);
                    } else if (this.dataType === Type.MAYORIGUAL) {
                        return new Value(tempL.value >= tempR.value, Type.BOOL, Type.VALOR, this.row, this.column);
                    } else if (this.dataType === Type.MENORIGUAL) {
                        return new Value(tempL.value <= tempR.value, Type.BOOL, Type.VALOR, this.row, this.column);
                    }
                } else if (tempL.dataType === Type.ENTERO && tempR.dataType === Type.CARACTER) {
                    /**
                     * Añadir o quitar else if segun los requeriemientos del lenguaje a interpretear
                     */
                    if (this.dataType === Type.IDENTICO) {
                        return new Value(tempL.value === tempR.value, Type.BOOL, Type.VALOR, this.row, this.column);
                    } else if (this.dataType === Type.DIFERENTE) {
                        return new Value(tempL.value !== tempR.value, Type.BOOL, Type.VALOR, this.row, this.column);
                    } else if (this.dataType === Type.MAYOR) {
                        return new Value(tempL.value > tempR.value, Type.BOOL, Type.VALOR, this.row, this.column);
                    } else if (this.dataType === Type.MENOR) {
                        return new Value(tempL.value < tempR.value, Type.BOOL, Type.VALOR, this.row, this.column);
                    } else if (this.dataType === Type.MAYORIGUAL) {
                        return new Value(tempL.value >= tempR.value, Type.BOOL, Type.VALOR, this.row, this.column);
                    } else if (this.dataType === Type.MENORIGUAL) {
                        return new Value(tempL.value <= tempR.value, Type.BOOL, Type.VALOR, this.row, this.column);
                    }
                } else if (tempL.dataType === Type.CARACTER && tempR.dataType === Type.ENTERO) {
                    /**
                     * Añadir o quitar else if segun los requeriemientos del lenguaje a interpretear
                     */
                    if (this.dataType === Type.IDENTICO) {
                        return new Value(tempL.value === tempR.value, Type.BOOL, Type.VALOR, this.row, this.column);
                    } else if (this.dataType === Type.DIFERENTE) {
                        return new Value(tempL.value !== tempR.value, Type.BOOL, Type.VALOR, this.row, this.column);
                    } else if (this.dataType === Type.MAYOR) {
                        return new Value(tempL.value > tempR.value, Type.BOOL, Type.VALOR, this.row, this.column);
                    } else if (this.dataType === Type.MENOR) {
                        return new Value(tempL.value < tempR.value, Type.BOOL, Type.VALOR, this.row, this.column);
                    } else if (this.dataType === Type.MAYORIGUAL) {
                        return new Value(tempL.value >= tempR.value, Type.BOOL, Type.VALOR, this.row, this.column);
                    } else if (this.dataType === Type.MENORIGUAL) {
                        return new Value(tempL.value <= tempR.value, Type.BOOL, Type.VALOR, this.row, this.column);
                    }
                } else if (tempL.dataType === Type.DECIMAL && tempR.dataType === Type.CARACTER) {
                    /**
                     * Añadir o quitar else if segun los requeriemientos del lenguaje a interpretear
                     */
                    if (this.dataType === Type.IDENTICO) {
                        return new Value(tempL.value === tempR.value, Type.BOOL, Type.VALOR, this.row, this.column);
                    } else if (this.dataType === Type.DIFERENTE) {
                        return new Value(tempL.value !== tempR.value, Type.BOOL, Type.VALOR, this.row, this.column);
                    } else if (this.dataType === Type.MAYOR) {
                        return new Value(tempL.value > tempR.value, Type.BOOL, Type.VALOR, this.row, this.column);
                    } else if (this.dataType === Type.MENOR) {
                        return new Value(tempL.value < tempR.value, Type.BOOL, Type.VALOR, this.row, this.column);
                    } else if (this.dataType === Type.MAYORIGUAL) {
                        return new Value(tempL.value >= tempR.value, Type.BOOL, Type.VALOR, this.row, this.column);
                    } else if (this.dataType === Type.MENORIGUAL) {
                        return new Value(tempL.value <= tempR.value, Type.BOOL, Type.VALOR, this.row, this.column);
                    }
                } else if (tempL.dataType === Type.CARACTER && tempR.dataType === Type.DECIMAL) {
                    /**
                     * Añadir o quitar else if segun los requeriemientos del lenguaje a interpretear
                     */
                    if (this.dataType === Type.IDENTICO) {
                        return new Value(tempL.value === tempR.value, Type.BOOL, Type.VALOR, this.row, this.column);
                    } else if (this.dataType === Type.DIFERENTE) {
                        return new Value(tempL.value !== tempR.value, Type.BOOL, Type.VALOR, this.row, this.column);
                    } else if (this.dataType === Type.MAYOR) {
                        return new Value(tempL.value > tempR.value, Type.BOOL, Type.VALOR, this.row, this.column);
                    } else if (this.dataType === Type.MENOR) {
                        return new Value(tempL.value < tempR.value, Type.BOOL, Type.VALOR, this.row, this.column);
                    } else if (this.dataType === Type.MAYORIGUAL) {
                        return new Value(tempL.value >= tempR.value, Type.BOOL, Type.VALOR, this.row, this.column);
                    } else if (this.dataType === Type.MENORIGUAL) {
                        return new Value(tempL.value <= tempR.value, Type.BOOL, Type.VALOR, this.row, this.column);
                    }
                } else if (tempL.dataType === Type.DECIMAL && tempR.dataType === Type.DECIMAL) {
                    /**
                     * Añadir o quitar else if segun los requeriemientos del lenguaje a interpretear
                     */
                    if (this.dataType === Type.IDENTICO) {
                        return new Value(tempL.value === tempR.value, Type.BOOL, Type.VALOR, this.row, this.column);
                    } else if (this.dataType === Type.DIFERENTE) {
                        return new Value(tempL.value !== tempR.value, Type.BOOL, Type.VALOR, this.row, this.column);
                    } else if (this.dataType === Type.MAYOR) {
                        return new Value(tempL.value > tempR.value, Type.BOOL, Type.VALOR, this.row, this.column);
                    } else if (this.dataType === Type.MENOR) {
                        return new Value(tempL.value < tempR.value, Type.BOOL, Type.VALOR, this.row, this.column);
                    } else if (this.dataType === Type.MAYORIGUAL) {
                        return new Value(tempL.value >= tempR.value, Type.BOOL, Type.VALOR, this.row, this.column);
                    } else if (this.dataType === Type.MENORIGUAL) {
                        return new Value(tempL.value <= tempR.value, Type.BOOL, Type.VALOR, this.row, this.column);
                    }
                } else if (tempL.dataType === Type.CARACTER && tempR.dataType === Type.CARACTER) {
                    /**
                     * Añadir o quitar else if segun los requeriemientos del lenguaje a interpretear
                     */
                    if (this.dataType === Type.IDENTICO) {
                        return new Value(tempL.value === tempR.value, Type.BOOL, Type.VALOR, this.row, this.column);
                    } else if (this.dataType === Type.DIFERENTE) {
                        return new Value(tempL.value !== tempR.value, Type.BOOL, Type.VALOR, this.row, this.column);
                    } else if (this.dataType === Type.MAYOR) {
                        return new Value(tempL.value > tempR.value, Type.BOOL, Type.VALOR, this.row, this.column);
                    } else if (this.dataType === Type.MENOR) {
                        return new Value(tempL.value < tempR.value, Type.BOOL, Type.VALOR, this.row, this.column);
                    } else if (this.dataType === Type.MAYORIGUAL) {
                        return new Value(tempL.value >= tempR.value, Type.BOOL, Type.VALOR, this.row, this.column);
                    } else if (this.dataType === Type.MENORIGUAL) {
                        return new Value(tempL.value <= tempR.value, Type.BOOL, Type.VALOR, this.row, this.column);
                    }
                } else if (tempL.dataType === Type.CADENA && tempR.dataType === Type.CADENA) {
                    /**
                     * Añadir o quitar else if segun los requeriemientos del lenguaje a interpretear
                     */
                    if (this.dataType === Type.IDENTICO) {
                        return new Value(tempL.value === tempR.value, Type.BOOL, Type.VALOR, this.row, this.column);
                    } else if (this.dataType === Type.DIFERENTE) {
                        return new Value(tempL.value !== tempR.value, Type.BOOL, Type.VALOR, this.row, this.column);
                    } else if (this.dataType === Type.REFERECIA) {
                        return new Value(tempL.value > tempR.value, Type.BOOL, Type.VALOR, this.row, this.column);
                    }
                } else if (tempL.dataType === Type.BOOL && tempR.dataType === Type.BOOL) {
                    /**
                     * Añadir o quitar else if segun los requeriemientos del lenguaje a interpretear
                     */
                    if (this.dataType === Type.IDENTICO) {
                        return new Value(tempL.value === tempR.value, Type.BOOL, Type.VALOR, this.row, this.column);
                    } else if (this.dataType === Type.DIFERENTE) {
                        return new Value(tempL.value !== tempR.value, Type.BOOL, Type.VALOR, this.row, this.column);
                    }
                }
            }

        }
        count.putOperatorError(this.dataType, this.row, this.column);
        return null;
    }


}

module.exports = Relational;