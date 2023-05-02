class Nodo_Arbol{
    constructor(valor, tipo){
        this.id=0;
        this.valor=valor;
        this.tipo=tipo;
        this.hijos=[];
    }
    getValor(){
        this.valor;
    }
    getTipo(){
        this.tipo;
    }
    agregarHijo(hijo){
        this.hijos.push(hijo);
    }

    recorrer_print(nodo){
        var concatena="";
        
        nodo.hijos.forEach(element =>{
            /* id -> id; */
            if(element.valor=="Ex"){                
                concatena+= this.recorrer_print(element);
            }else{
                concatena+= nodo.hijos[0].valor +'';
            }       
            
        });
        return concatena;
    }
}

