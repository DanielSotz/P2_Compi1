const Type = require('./Type');

class Counter {

    constructor(){
        this.clearAll();
    }

    clearAll() {
        this.output = '';
        this.errors = '';
        this.Rsymbol = [];
        this.Rerror = [];
    }
    getOutput() {
        return this.output;
    }

    getErrorsTable(){
        return this.Rerror;
    }

    getSymbolsTable() {
        return this.Rsymbol;
    }

    putSymbol(_id, _dataType, _structureType, _scopeType, _value) {
        this.Rsymbol.push({id: _id, dataType: _dataType, structureType: _structureType, scopeType: _scopeType, value: _value});
    }

    /**
     * 
     * @param {Type} type 
     * @param {string} instruction 
     * @param {int} row 
     * @param {int} column 
     * 
     * Se usa para crear un nuevo error de manera directa
     */
    putError(type, instruction, row, column) {
        this.Rerror.push({type: type, message: instruction, row: row, column: column});
    }

    /**
     * 
     * @param {Type} type 
     * @param {string} instruction 
     * @param {int} row 
     * @param {int} column 
     * 
     * Se usa para crear un nuevo error de manera automatica si utiliza el mismo mensaje varias veces
     */
    putOperatorError(operator, row, col) {
        let message = "No se puede ejecutar la operacion " + operator.toString() + ", No reconocida o No Permitida."
        this.putError(Type.SEMANTICO, message, row, col);
    }
}

module.exports = Counter;