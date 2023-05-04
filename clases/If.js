//import Expression from './Expression';

class If {
    constructor(e, c, _dataType, _row, _column) {
        this.exp = e;
        this.body = c;
        this.dataType = _dataType;
        this.row = _row;
        this.column = _column;
    }
}

module.exports = If;