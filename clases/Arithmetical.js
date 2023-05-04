//import Expression from './Expression';
const Type = require('./Type')
const Value = require('./Value');

class Arithmetical {

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
        if (this.node_left != null) {
            tempL = this.node_left.operate(tab, count);

        }
        if (this.node_right != null) {
            tempR = this.node_right.operate(tab, count);

        }

        if (tempR != null && tempL != null) {
            /**
             * Si los atributos son validos entonces se ejecuta la logica de la instruccion
             * en este caso se realizaran las siguientes operaciones:
             * - Suma
             * - Resta
             * - Multiplicacion
             * - Division
             * - Potencia
             * - Moodulo
             * Para las diferentes combinaciones de tipos de datos
             */
            if (tempR.structureType === Type.VALOR && tempL.structureType === Type.VALOR) {
                /**
                 * Combinaciones de los Operando izquierdo y derecho
                 */
                if (tempL.dataType === Type.ENTERO && tempR.dataType === Type.ENTERO) {
                    if (null != this.dataType) {
                        /**
                         * Añadir o quitar case segun los requeriemientos del lenguaje a interpretear
                         */
                        switch (this.dataType) {
                            case Type.SUMA:
                                return new Value(tempL.value + tempR.value, Type.ENTERO, Type.VALOR, this.row, this.column);
                            case Type.RESTA:
                                return new Value(tempL.value - tempR.value, Type.ENTERO, Type.VALOR, this.row, this.column);
                            case Type.MULTIPLICACION:
                                return new Value(tempL.value * tempR.value, Type.ENTERO, Type.VALOR, this.row, this.column);
                            case Type.DIVISION:
                                return new Value(tempL.value / tempR.value, Type.ENTERO, Type.VALOR, this.row, this.column);
                            case Type.POTENCIA:
                                return new Value(Math.pow(tempL.value, tempR.value), Type.DECIMAL, Type.VALOR, this.row, this.column);
                            case Type.MODULO:
                                return new Value(tempL.value % tempR.value, Type.ENTERO, Type.VALOR, this.row, this.column);
                            default:
                                count.putOperatorError(this.dataType, this.row, this.column);
                                break;
                        }
                    }
                    return null;
                } else if (tempL.dataType === Type.ENTERO && tempR.dataType === Type.DECIMAL) {
                    if (null != this.dataType) {
                        /**
                         * Añadir o quitar case segun los requeriemientos del lenguaje a interpretear
                         */
                        switch (this.dataType) {
                            case Type.SUMA:
                                return new Value(tempL.value + tempR.value, Type.DECIMAL, Type.VALOR, this.row, this.column);
                            case Type.RESTA:
                                return new Value(tempL.value - tempR.value, Type.DECIMAL, Type.VALOR, this.row, this.column);
                            case Type.MULTIPLICACION:
                                return new Value(tempL.value * tempR.value, Type.DECIMAL, Type.VALOR, this.row, this.column);
                            case Type.DIVISION:
                                return new Value(tempL.value / tempR.value, Type.DECIMAL, Type.VALOR, this.row, this.column);
                            default:
                                count.putOperatorError(this.dataType, this.row, this.column);
                                break;
                        }
                    }
                    return null;
                } else if (tempL.dataType === Type.DECIMAL && tempR.dataType === Type.ENTERO) {
                    if (null != this.dataType) {
                        /**
                         * Añadir o quitar case segun los requeriemientos del lenguaje a interpretear
                         */
                        switch (this.dataType) {
                            case Type.SUMA:
                                return new Value(tempL.value + tempR.value, Type.DECIMAL, Type.VALOR, this.row, this.column);
                            case Type.RESTA:
                                return new Value(tempL.value - tempR.value, Type.DECIMAL, Type.VALOR, this.row, this.column);
                            case Type.MULTIPLICACION:
                                return new Value(tempL.value * tempR.value, Type.DECIMAL, Type.VALOR, this.row, this.column);
                            case Type.DIVISION:
                                return new Value(tempL.value / tempR.value, Type.DECIMAL, Type.VALOR, this.row, this.column);
                            default:
                                count.putOperatorError(this.dataType, this.row, this.column);
                                break;
                        }
                    }
                    return null;
                } else if (tempL.dataType === Type.ENTERO && tempR.dataType === Type.CARACTER) {
                    if (null != this.dataType) {
                        /**
                         * Añadir o quitar case segun los requeriemientos del lenguaje a interpretear
                         */
                        switch (this.dataType) {
                            case Type.SUMA:
                                return new Value(tempL.value + tempR.value, Type.ENTERO, Type.VALOR, this.row, this.column);
                            case Type.RESTA:
                                return new Value(tempL.value - tempR.value, Type.ENTERO, Type.VALOR, this.row, this.column);
                            case Type.MULTIPLICACION:
                                return new Value(tempL.value * tempR.value, Type.ENTERO, Type.VALOR, this.row, this.column);
                            case Type.DIVISION:
                                return new Value(tempL.value / tempR.value, Type.DECIMAL, Type.VALOR, this.row, this.column);
                            default:
                                count.putOperatorError(this.dataType, this.row, this.column);
                                break;
                        }
                    }
                    return null;
                } else if (tempL.dataType === Type.CARACTER && tempR.dataType === Type.ENTERO) {
                    if (null != this.dataType) {
                        /**
                         * Añadir o quitar case segun los requeriemientos del lenguaje a interpretear
                         */
                        switch (this.dataType) {
                            case Type.SUMA:
                                return new Value(tempL.value + tempR.value, Type.ENTERO, Type.VALOR, this.row, this.column);
                            case Type.RESTA:
                                return new Value(tempL.value - tempR.value, Type.ENTERO, Type.VALOR, this.row, this.column);
                            case Type.MULTIPLICACION:
                                return new Value(tempL.value * tempR.value, Type.ENTERO, Type.VALOR, this.row, this.column);
                            case Type.DIVISION:
                                return new Value(tempL.value / tempR.value, Type.ENTERO, Type.VALOR, this.row, this.column);
                            default:
                                count.putOperatorError(this.dataType, this.row, this.column);
                                break;
                        }
                    }
                    return null;
                } else if (tempL.dataType === Type.ENTERO && tempR.dataType === Type.CADENA) {
                    if (null != this.dataType) {
                        /**
                         * Añadir o quitar case segun los requeriemientos del lenguaje a interpretear
                         */
                        switch (this.dataType) {
                            case Type.SUMA:
                                return new Value(tempL.value + tempR.value, Type.CADENA, Type.VALOR, this.row, this.column);
                            default:
                                count.putOperatorError(this.dataType, this.row, this.column);
                                break;
                        }
                    }
                    return null;
                } else if (tempL.dataType === Type.CADENA && tempR.dataType === Type.ENTERO) {
                    if (null != this.dataType) {
                        /**
                         * Añadir o quitar case segun los requeriemientos del lenguaje a interpretear
                         */
                        switch (this.dataType) {
                            case Type.SUMA:
                                return new Value(tempL.value + tempR.value, Type.CADENA, Type.VALOR, this.row, this.column);
                            default:
                                count.putOperatorError(this.dataType, this.row, this.column);
                                break;
                        }
                    }
                    return null;
                } else if (tempL.dataType === Type.DECIMAL && tempR.dataType === Type.CADENA) {
                    if (this.dataType === Type.SUMA) {
                        return new Value(tempL.value + tempR.value.toString(), Type.CADENA, Type.VALOR, this.row, this.column);
                    }
                    count.putOperatorError(this.dataType, this.row, this.column);
                    return null;
                } else if (tempL.dataType === Type.CADENA && tempR.dataType === Type.DECIMAL) {
                    if (this.dataType === Type.SUMA) {
                        return new Value(tempL.value + tempR.value.toString(), Type.CADENA, Type.VALOR, this.row, this.column);
                    }
                    count.putOperatorError(this.dataType, this.row, this.column);
                    return null;
                } else if (tempL.dataType === Type.DECIMAL && tempR.dataType === Type.CARACTER) {
                    if (null != this.dataType) {
                        /**
                         * Añadir o quitar case segun los requeriemientos del lenguaje a interpretear
                         */
                        switch (this.dataType) {
                            case Type.SUMA:
                                return new Value(tempL.value + tempR.value, Type.DECIMAL, Type.VALOR, this.row, this.column);
                            case Type.RESTA:
                                return new Value(tempL.value - tempR.value, Type.DECIMAL, Type.VALOR, this.row, this.column);
                            case Type.MULTIPLICACION:
                                return new Value(tempL.value * tempR.value, Type.DECIMAL, Type.VALOR, this.row, this.column);
                            case Type.DIVISION:
                                return new Value(tempL.value / tempR.value, Type.DECIMAL, Type.VALOR, this.row, this.column);
                            default:
                                count.putOperatorError(this.dataType, this.row, this.column);
                                break;
                        }
                    }
                    return null;
                } else if (tempL.dataType === Type.CARACTER && tempR.dataType === Type.DECIMAL) {
                    if (null != this.dataType) {
                        /**
                         * Añadir o quitar case segun los requeriemientos del lenguaje a interpretear
                         */
                        switch (this.dataType) {
                            case Type.SUMA:
                                return new Value(tempL.value + tempR.value, Type.DECIMAL, Type.VALOR, this.row, this.column);
                            case Type.RESTA:
                                return new Value(tempL.value - tempR.value, Type.DECIMAL, Type.VALOR, this.row, this.column);
                            case Type.MULTIPLICACION:
                                return new Value(tempL.value * tempR.value, Type.DECIMAL, Type.VALOR, this.row, this.column);
                            case Type.DIVISION:
                                return new Value(tempL.value / tempR.value, Type.DECIMAL, Type.VALOR, this.row, this.column);
                            default:
                                count.putOperatorError(this.dataType, this.row, this.column);
                                break;
                        }
                    }
                    return null;
                } else if (tempL.dataType === Type.DECIMAL && tempR.dataType === Type.DECIMAL) {
                    if (null != this.dataType) {
                        /**
                         * Añadir o quitar case segun los requeriemientos del lenguaje a interpretear
                         */
                        switch (this.dataType) {
                            case Type.SUMA:
                                return new Value(tempL.value + tempR.value, Type.DECIMAL, Type.VALOR, this.row, this.column);
                            case Type.RESTA:
                                return new Value(tempL.value - tempR.value, Type.DECIMAL, Type.VALOR, this.row, this.column);
                            case Type.MULTIPLICACION:
                                return new Value(tempL.value * tempR.value, Type.DECIMAL, Type.VALOR, this.row, this.column);
                            case Type.DIVISION:
                                return new Value(tempL.value / tempR.value, Type.DECIMAL, Type.VALOR, this.row, this.column);
                            default:
                                count.putOperatorError(this.dataType, this.row, this.column);
                                break;
                        }
                    }
                    return null;
                } else if (tempL.dataType === Type.CARACTER && tempR.dataType === Type.CARACTER) {
                    if (null != this.dataType) {
                        /**
                         * Añadir o quitar case segun los requeriemientos del lenguaje a interpretear
                         */
                        switch (this.dataType) {
                            case Type.SUMA:
                                return new Value(tempL.value + tempR.value, Type.CADENA, Type.VALOR, this.row, this.column);
                            case Type.RESTA:
                                return new Value(tempL.value - tempR.value, Type.ENTERO, Type.VALOR, this.row, this.column);
                            case Type.MULTIPLICACION:
                                return new Value(tempL.value * tempR.value, Type.ENTERO, Type.VALOR, this.row, this.column);
                            case Type.DIVISION:
                                return new Value(tempL.value / tempR.value, Type.DECIMAL, Type.VALOR, this.row, this.column);
                            default:
                                count.putOperatorError(this.dataType, this.row, this.column);
                                break;
                        }
                    }
                    return null;
                } else if (tempL.dataType === Type.CARACTER && tempR.dataType === Type.CADENA) {
                    if (this.dataType === Type.SUMA) {
                        return new Value(tempL.value + tempR.value.toString(), Type.CADENA, Type.VALOR, this.row, this.column);
                    }
                    count.putOperatorError(this.dataType, this.row, this.column);
                    return null;
                } else if (tempL.dataType === Type.CADENA && tempR.dataType === Type.CARACTER) {
                    if (this.dataType === Type.SUMA) {
                        return new Value(tempL.value + tempR.value.toString(), Type.CADENA, Type.VALOR, this.row, this.column);
                    }
                    count.putOperatorError(this.dataType, this.row, this.column);
                    return null;
                } else if (tempL.dataType === Type.CADENA && tempR.dataType === Type.CADENA) {
                    if (this.dataType === Type.SUMA) {
                        return new Value(tempL.value.toString() + tempR.value.toString(), Type.CADENA, Type.VALOR, this.row, this.column);
                    }
                    count.putOperatorError(this.dataType, this.row, this.column);
                    return null;
                } else if (tempL.dataType === Type.CADENA && tempR.dataType === Type.BOOL) {
                    if (this.dataType === Type.SUMA) {
                        return new Value(tempL.value.toString() + tempR.value.toString(), Type.CADENA, Type.VALOR, this.row, this.column);
                    }
                    count.putOperatorError(this.dataType, this.row, this.column);
                    return null;
                } else if (tempL.dataType === Type.BOOL && tempR.dataType === Type.CADENA) {
                    if (this.dataType === Type.SUMA) {
                        return new Value(tempL.value.toString() + tempR.value.toString(), Type.CADENA, Type.VALOR, this.row, this.column);
                    }
                    count.putOperatorError(this.dataType, this.row, this.column);
                    return null;
                }
            }
        }
        /**
         * Marcar mensaje de error u 
         * Retorno en caso de no poder ejecutar nada.
         */
        count.putOperatorError("null", this.row, this.column);
        return null;
    }

}

module.exports = Arithmetical;