class Symbol {
    
    constructor(_id, _dataType, _structureType, _scopeType, _value){
        this.dataType = _dataType; // tipo de dato - INTEGER, DOUBLE, DECIMAL, STRING, OBJETO, etc.
        this.structureType = _structureType; // tipo estructura - VALOR, ARREGLO, LISTA, etc.
        this.scopeType = _scopeType; // tipo de ambito - GLOBAL o LOCAL.
        /**
         * Se pueden añadir mas tipos si es necesario, por ejemplo si se va a 
         * utilizar constantes
         */
        this.id = _id;
        this.value = _value;
    }
}

module.exports = Symbol;