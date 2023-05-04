//import Expression from './Expression';
const Type = require('./Type');

class Assignment {
    constructor(_id, _val, _row, _column) {
        this.value = _val;
        this.id = _id;
        this.row = _row;
        this.column = _column;
    }

    /**
     * 
     * @param {SymbolTable} tab tabla de simbolos del ambito actual
     * @param {Counters} count Objeto para sustituir variables globales
     * @returns null
     * 
     * La instruccion asignacion consta de la siguinete estructura
     * <identificador> = <expresion>;
     */

    operate(tab, count) {
        /**
         * Se busca la variable dentro de la lista de tablas de simbolos
         */
        let a = tab.getSymbol(this.id);
        if (a == null) {
            /**
             * Si no se encontro la variable se marca error por que aun no ha sido declarada esa variable
             */
            count.putError(Type.SEMANTICO, "Variable " + this.id + " no encontrada.", this.row, this.column);
            return null;
        }
        /**
         * Se manda a operar la expresion a asignar en la variable
         */
        let tmpExp = this.value.operate(tab, count);

        if (tmpExp == null) {
            /**
             * Si la expresion es null hubo un error al operarla, entonces no se ejecuta nada
             */
            count.putError(Type.SEMANTICO, "Hubo un error al realizar la asignacion de la variable " + this.id + ".", this.row, this.column);
            return null;
        }
        /**
         * Verificamos que el tipo de dato del simbolo sea el mismo que la expresion operada previamente
         */
        if (this.checkType(tmpExp, a.dataType, count)) {
            return null;
        }
        /**
         * Si los tipos coinciden esta todo correcto y se puede proceder a hacer el cambio de valor
         * en el simbolo de la tabla de simbolo
         */
        a.structureType = tmpExp.structureType;
        a.dataType = tmpExp.dataType;
        a.value = tmpExp.value;
        return true;
    }

    checkType(val, type, cont) {
        if (type !== val.dataType) {
            cont.putError(Type.SEMANTICO, 'Tipo de la EXPRESION ' + val.dataType + ' No Asignable a un ' + type + '.', this.row, this.column);
            return true;
        }
        return false;
    }

}

module.exports = Assignment;