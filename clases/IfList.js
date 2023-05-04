//import Expression from './Expression';
const Type = require('./Type');
const SymbolTable = require('./SymbolTable');

class IfList {
	constructor() {
		this.lif = []; //lista de objetos If
		this.elsebody = [];
	}

	/**
     * 
     * @param {SymbolTable} tab tabla de simbolos del ambito actual
     * @param {Counters} count Objeto para sustituir variables globales
     * @returns null
     * 
     * La instruccion IfLis consta de la siguinete estructura
     * es utilizado para ejecutar un if y else if
	 * o tambien para ejecutar un switch case
     */
	operate(tab, count) {

		/**
		 * Se recorre toda la lista de objetos if hasta que la expresion de un If de como resultado un true
		 */
		for (let j = 0; j < this.lif.length; j++) {
			/**
			 * Se evalua la expresion del If actual y se comprueba que no de errores
			 */
			let r = this.lif[j].exp.operate(tab, count);
			if (r == null) {
				count.putError(Type.SINTACTICO, "No se puede ejecutar la instruccion If, se necesita una condicion logica o relacional.", this.lif[j].row, this.lif[j].column);
				return null;
			}
			if (r.dataType !== Type.BOOL) {
				count.putError(Type.SINTACTICO, "No se puede ejecutar la operacion " + r.dataType + ", se necesita una condicion logica o relacional.", this.lif[j].row, this.lif[j].column);
			}

			/**
			 * Si la expresion arroja un valor true entonces se ejecuta el conjunto de instrucciones
			 */
			if (r.value === true) {
				/**
				 * Se crea una nueva tabla de simbolo debido a que se usara un nuevo ambito
				 * se crea una nueva tabla de simbolos, se esta creando un ambito nuevo y se esta añadiendo un nodo nuevo
				 * a las lista de tablas de simbolos
				 */
				let s = new SymbolTable(tab);
				for (let i = 0; i < this.lif[j].body.length; i++) {
					/**
					 * Se ejecuta el bloque de instrucciones dentro del If
					 * con la nueva tabla de simbolos
					 */
					this.lif[j].body[i].operate(s, count); //<= tabla de simbolos creada para el nuevo ambito
				}
				return true;
			}

		}
		/**
		 * Si ningun if da como resultado true entonces se ejecuta por defecto si existe la instruccion Else
		 */
		if (this.elsebody != null) {
			let body = this.elsebody.body;
			/**
			 * Se crea una nueva tabla de simbolo debido a que se usara un nuevo ambito
			 * se crea una nueva tabla de simbolos, se esta creando un ambito nuevo y se esta añadiendo un nodo nuevo
			 * a las lista de tablas de simbolos
			 */
			let s = new SymbolTable(tab);
			for (let i = 0; i < body.length; i++) {

				body[i].operate(s, count); //<= tabla de simbolos creada para el nuevo ambito

			}
			return true;
		}
		return null;
	}

}

module.exports = IfList;