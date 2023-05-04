//import Expression from './Expression';

class Function {

    constructor(_dataType, _structureType, _id, _param, _body, _row, _col) {
        this.dataType = _dataType;
        this.structureType = _structureType;
        this.id = _id;
        if (_param == null) {
            this.param = []
        } else {
            this.param = _param;
        }
        this.body = _body;
        this.row = _row;
        this.column = _col;
        this.symbolTab = null;
    }

    operate(tab, count) {
        return null;
    }

}

module.exports = Function;