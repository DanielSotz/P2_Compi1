%{
	
%}

%lex
%options case-insensitive
%%


"//".*                  // comentario simple
[/][*][^*]*[*]+([^/*][^*]*[*]+)*[/]     //comentario multiple

"expresion"     return 'tk_expresion';
"print"         return 'PRINT';
"toLower"       return 'LOWER';
"toUpper"       return 'UPPER';
"length"        return 'LENGTH';
"truncate"      return 'TRUNCATE';
"round"         return 'ROUND';
"typeof"        return 'TYPEOF';
"toString"      return 'TOSTRING';
"toCharArray"   return 'CHARARRAY';
"exec"          return 'EXEC';
"new"           return 'NEW';

"int"           return 'RENTERO';
"double"        return 'RDOUBLE';
"boolean"       return 'RBOOLEANO';
"char"          return 'RCARACTER';
"string"        return 'RCADENA';

\\\n            return 'SALTOLINEA';
\\\\            return 'BARINVERSA';
\\\'            return 'COMILLASSIM';
\\\"            return 'COMILLASDOB';
\\\t            return 'TABULAR';

"while"         return 'WHILE';
"do"            return 'DO';
"if"            return 'IF';
"else"          return 'ELSE';
"for"           return 'FOR';
"switch"        return 'SWITCH';
"case"          return 'CASE';
"default"       return 'DEFAULT';

"break"         return 'BREAK';
"continue"      return 'CONTINUE';
"return"        return 'RETURN';

"void"          return 'VOID';
"main"          return 'MAIN';

":"             return 'DOSPTS';
","             return 'COMA';
";"             return 'PTCOMA';
"{"             return 'LLAVIZQ';
"}"             return 'LLAVDER';
"("             return 'PARIZQ';
")"             return 'PARDER';
"["             return 'CORIZQ';
"]"             return 'CORDER';

"+="             return 'O_MAS';
"-="             return 'O_MENOS';
"*="             return 'O_POR';
"/="             return 'O_DIVIDIDO';

"++"            return 'INCREMENTO';
"--"            return 'DECREMENTO';
"+"             return 'MAS';
"-"             return 'MENOS';
"*"             return 'POR';
"/"             return 'DIVIDIDO';
"^"             return 'POTENCIA';
"%"             return 'MOD';

"<="            return 'MENIGQUE';
">="            return 'MAYIGQUE';
"=="            return 'DOBLEIGUAL';
"!="            return 'NOIGUAL';
"<"             return 'MENQUE';
">"             return 'MAYQUE';

"!"             return 'NOT';
"&&"            return 'AND';
"||"            return 'OR';

"true"          return 'TRUE';
"false"         return 'FALSE';

"="             return 'IGUAL';
"?"             return 'OPTERNARIO';
"&"             return 'CONCAT';

\s+                 // se ignoran espacios en blanco

\'([^']|"\\n"|"\\r"|"\\t")\'             return 'CARACTER';
([\"]("\\\""|[^"])*[^\\][\"])|[\"][\"]   return 'CADENA';


[0-9]+("."[0-9]+)\b         return 'DECIMAL';
[0-9]+\b                    return 'ENTERO';
([a-zA-Z])[a-zA-Z0-9_]*     return 'IDENTIFICADOR';


<<EOF>>                     return 'EOF';

.                           {document.getElementById("txtsalida1"+publico_id).value+='Error Lexico: ' + yytext + ', en la linea: ' + yylloc.first_line + ', en la columna: ' + yylloc.first_column +"\n";}
/lex

%left 'OR'
%left 'AND'
%right 'NOT'
%left  'DOBLEIGUAL' 'NOIGUAL' 'MENQUE' 'MAYQUE' 'MENIGQUE' 'MAYIGQUE'
%left   'MAS' 'MENOS'
%left   'POR' 'DIVIDIDO' 'MOD'
%nonassoc   'POTENCIA'
%right UMENOS

%start inicio
%%


inicio:     
        instrucciones EOF   { $$= new Nodo_Arbol("INICIO","");
                             $$.agregarHijo($1);
                             return $$; }
;
instrucciones:
          instrucciones  instruccion { $$= new Nodo_Arbol("INSTRUCCIONES","");
                                    $$.agregarHijo($1);
                                    $$.agregarHijo($2);}
        | instruccion                { $$= new Nodo_Arbol("INSTRUCCION","");
                                        $$.agregarHijo($1);  }
;
instruccion 
    : PRINT PARIZQ expresion PARDER PTCOMA                  { $$ = new Nodo_Arbol("PRINT","");                                                    
                                                                $$.agregarHijo($3);
                                                                $$.tipo=$3.tipo; 																
																var texto= $$.recorrer_print($3);
                                                                texto = texto.replace(/Ex/g, "");
												document.getElementById("txtsalida1"+publico_id).value+=" "+ texto+"\n"; }
    |DECLARACION                                           { $$ = $1 }
    |ASIGNACION                                            { $$ = $1 }
    |FUNCION                                               { $$ = $1 }
    |LLAMADA                                               { $$ = $1 }
    |condIF                                                { $$ = $1 }
    |switchCASE                                            { $$ = $1 }
    |condWHILE                                             { $$ = $1 }
    |condDOWHILE                                           { $$ = $1 }
    |condFOR                                               { $$ = $1 }
    |IMAIN                                                 { $$ = $1 }
    |IDENTIFICADOR INCREMENTO PTCOMA		               { $$ = new Nodo_Arbol("INCREMENTAR","");
                                                              $$.agregarHijo(new Nodo_Arbol($1,""));
                                                              $$.agregarHijo(new Nodo_Arbol("++",""));  }
    | IDENTIFICADOR DECREMENTO PTCOMA		                { $$ = new Nodo_Arbol("INCREMENTAR","");
                                                              $$.agregarHijo(new Nodo_Arbol($1,""));
                                                              $$.agregarHijo(new Nodo_Arbol("--",""));}
    |BREAK PTCOMA                                          { $$ = new Nodo_Arbol("BREAK","");}
    |CONTINUE PTCOMA                                          { $$ = new Nodo_Arbol("BREAK","");}
    | error  { document.getElementById("txtsalida1"+publico_id).value+="Error sintactico en la Linea: " + this._$.first_line + " en la Columna: " + this._$.first_column+"\n";
    //Salida.push('Este es un error Sintactico: ' + yytext + ', en la linea: ' + this.$.first_line + ', en la columna: ' + this.$.first_line.first_column);
                    //errores.push(new Error_(this.$.first_line,this.$.first_column,"Sintactico","No se esperaba la expresion: " + yytext));
                    //console.log('Este es un error Sintactico: ' + yytext + ', en la linea: ' + yylineno.first_line + ', en la columna: ' + yylineno.first_column);
                    }
;

IMAIN
    : MAIN IDENTIFICADOR PARIZQ L_exp PARDER PTCOMA              { $$ = new Nodo_Arbol("MAIN","");
                                                                $$.agregarHijo(new Nodo_Arbol($2,""));
                                                                $$.agregarHijo($4);}
    | MAIN IDENTIFICADOR PARIZQ PARDER PTCOMA                   { $$ = new Nodo_Arbol("MAIN","");
                                                                $$.agregarHijo(new Nodo_Arbol($2,""));}

;

DECLARACION 
    : TIPO IDENTIFICADOR IGUAL expresion  PTCOMA	        { $$ = new Nodo_Arbol("DECLARACION","");
                                                               $$.agregarHijo($1);                                                               
                                                               $$.agregarHijo(new Nodo_Arbol($2,""));
                                                               $$.agregarHijo($4); }
    | TIPO IDENTIFICADOR PTCOMA	                            { $$ = new Nodo_Arbol("DECLARACION","");
                                                               $$.agregarHijo($1);                                                               
                                                               $$.agregarHijo(new Nodo_Arbol($2,""));
                                                            }
;

ASIGNACION 
    :IDENTIFICADOR IGUAL expresion  PTCOMA	        { $$ = new Nodo_Arbol("ASIGNACION","");                                                                                                                             
                                                               $$.agregarHijo(new Nodo_Arbol($1,""));
                                                               $$.agregarHijo($3); }
;

TIPO 
    : RENTERO               { $$ = new Nodo_Arbol("REntero","");
                              //$$.agregarHijo(new Nodo_Arbol($1,"identificador")); 
                              }
    | Rbooleano             { $$ = new Nodo_Arbol("RBooleano",""); }   
    | RCADENA               { $$ = new Nodo_Arbol("RCadena","");}   
    | RCARACTER             { $$ = new Nodo_Arbol("RCaracter",""); }   
    | RDOUBLE               { $$ = new Nodo_Arbol("RDouble",""); }   
;

FUNCION
    : TIPO IDENTIFICADOR PARIZQ PARDER BLOQUE                       { $$ = new Nodo_Arbol("FUNCION","");
                                                                        $$.agregarHijo($1);                                                               
                                                                        $$.agregarHijo(new Nodo_Arbol($2,""));
                                                                        $$.agregarHijo($5); }
    | VOID IDENTIFICADOR PARIZQ PARDER BLOQUE                       { $$ = new Nodo_Arbol("FUNCION","");
                                                                        $$.agregarHijo(new Nodo_Arbol("RVOID",""));                                                               
                                                                        $$.agregarHijo(new Nodo_Arbol($2,""));
                                                                        $$.agregarHijo($5); }
    | TIPO IDENTIFICADOR PARIZQ PARAMETROS PARDER BLOQUE            { $$ = new Nodo_Arbol("FUNCION","");
                                                                        $$.agregarHijo($1);                                                               
                                                                        $$.agregarHijo(new Nodo_Arbol($2,""));
                                                                        $$.agregarHijo($4);
                                                                        $$.agregarHijo($6); }
    | VOID IDENTIFICADOR PARIZQ PARAMETROS PARDER BLOQUE            { $$ = new Nodo_Arbol("FUNCION","");
                                                                        $$.agregarHijo(new Nodo_Arbol("RVOID",""));                                                               
                                                                        $$.agregarHijo(new Nodo_Arbol($2,""));
                                                                        $$.agregarHijo($4);
                                                                        $$.agregarHijo($6); }
;

PARAMETROS
    : PARAMETROS COMA TIPO IDENTIFICADOR                        { $$ = new Nodo_Arbol("PARAMETROS","");
                                                                    $$.agregarHijo($1);
                                                                    $$.agregarHijo($3);                                                               
                                                                    $$.agregarHijo(new Nodo_Arbol($4,""));    }
    | TIPO IDENTIFICADOR                                        { $$ = new Nodo_Arbol("PARAMETRO","");
                                                                    $$.agregarHijo($1);                                                               
                                                                    $$.agregarHijo(new Nodo_Arbol($2,"")); }
;

BLOQUE
    : LLAVIZQ LLAVDER                       { $$ = new Nodo_Arbol("BLOQUE_VACIO",""); }
    | LLAVIZQ instrucciones LLAVDER         { $$ = new Nodo_Arbol("BLOQUE","");
                                                $$.agregarHijo($2);      }
;

LLAMADA
    : IDENTIFICADOR PARIZQ PARDER  PTCOMA                         { $$ = new Nodo_Arbol("LLAMADA","");                                                                                                                             
                                                                        $$.agregarHijo(new Nodo_Arbol($1,""));
                                                                    }
    | IDENTIFICADOR PARIZQ L_exp PARDER  PTCOMA                   { $$ = new Nodo_Arbol("LLAMADA","");                                                                                                                             
                                                                        $$.agregarHijo(new Nodo_Arbol($1,""));
                                                                        $$.agregarHijo($3); }
;

condIF
    : IF expresion BLOQUE                   { $$ = new Nodo_Arbol("IF","");
                                                $$.agregarHijo(new Nodo_Arbol("RIF",""));
                                                $$.agregarHijo($2); 
                                                $$.agregarHijo($3); }
    | IF expresion BLOQUE ELSE condIF   { $$ = new Nodo_Arbol("IF","");
                                                $$.agregarHijo(new Nodo_Arbol("RIF",""));
                                                $$.agregarHijo($2); 
                                                $$.agregarHijo($3);
                                                $$.agregarHijo(new Nodo_Arbol("RELSE",""));
                                                $$.agregarHijo($5);}
    | IF expresion BLOQUE ELSE BLOQUE       { $$ = new Nodo_Arbol("IF","");
                                                $$.agregarHijo(new Nodo_Arbol("RIF",""));
                                                $$.agregarHijo($2); 
                                                $$.agregarHijo($3);
                                                $$.agregarHijo(new Nodo_Arbol("RELSE",""));
                                                $$.agregarHijo($5); }
;

switchCASE
    : SWITCH expresion LLAVIZQ ListCase LLAVDER                    { $$ = new Nodo_Arbol("SWITCH","");
                                                                        $$.agregarHijo(new Nodo_Arbol("RSWITCH",""));
                                                                        $$.agregarHijo($2);
                                                                        $$.agregarHijo($4);}
    | SWITCH expresion LLAVIZQ ListCase DEFAULT BLOQUECASE LLAVDER { $$ = new Nodo_Arbol("SWITCH","");
                                                                        $$.agregarHijo(new Nodo_Arbol("RSWITCH",""));
                                                                        $$.agregarHijo($2);
                                                                        $$.agregarHijo($4);
                                                                        $$.agregarHijo($6); } 
;

ListCase
    : ListCase CASE expresion  BLOQUECASE   { $$ = new Nodo_Arbol("LCASE","");
                                                $$.agregarHijo($1);
                                                $$.agregarHijo(new Nodo_Arbol("RCASE",""));
                                                $$.agregarHijo($3);
                                                $$.agregarHijo($4);}
    | CASE expresion BLOQUECASE             { $$ = new Nodo_Arbol("CASE","");
                                                $$.agregarHijo(new Nodo_Arbol("RCASE",""));
                                                $$.agregarHijo($2);
                                                $$.agregarHijo($3); }
;

BLOQUECASE
    : DOSPTS                          { $$ = new Nodo_Arbol("BLOQUE_VACIO",""); }
    | DOSPTS instrucciones            { $$ = new Nodo_Arbol("BLOQUE","");
                                            $$.agregarHijo($2); }
;

condWHILE
    : WHILE expresion BLOQUE                { $$ = new Nodo_Arbol("WHILE","");
                                                $$.agregarHijo(new Nodo_Arbol("RWHILE",""));
                                                $$.agregarHijo($2); 
                                                $$.agregarHijo($3);}
;

condDOWHILE
    : DO BLOQUE WHILE expresion PTCOMA               {$$ = new Nodo_Arbol("DOWHILE","");
                                                $$.agregarHijo(new Nodo_Arbol("DO",""));
                                                $$.agregarHijo($2);
                                                $$.agregarHijo(new Nodo_Arbol("WHILE","")); 
                                                $$.agregarHijo($4);}
;

condFOR
    : FOR PARIZQ DECLARACION expresion PTCOMA refeshFOR PARDER  BLOQUE       { $$ = new Nodo_Arbol("FOR","");
                                                                                $$.agregarHijo(new Nodo_Arbol("RFOR",""));
                                                                                $$.agregarHijo($3);
                                                                                $$.agregarHijo($4);
                                                                                $$.agregarHijo($6);
                                                                                $$.agregarHijo($8);}
    | FOR PARIZQ ASIGNACION  expresion PTCOMA refeshFOR PARDER  BLOQUE       { $$ = new Nodo_Arbol("FOR","");
                                                                                $$.agregarHijo(new Nodo_Arbol("RFOR",""));
                                                                                $$.agregarHijo($3);
                                                                                $$.agregarHijo($4);
                                                                                $$.agregarHijo($6);
                                                                                $$.agregarHijo($8);}
;

refeshFOR
    : IDENTIFICADOR INCREMENTO				            { $$ = new Nodo_Arbol("RFOR","");
                                                          $$.agregarHijo(new Nodo_Arbol($1,""));
                                                          $$.agregarHijo(new Nodo_Arbol("++","")); }
    | IDENTIFICADOR DECREMENTO				            {  $$ = new Nodo_Arbol("RFOR","");
                                                            $$.agregarHijo(new Nodo_Arbol($1,""));
                                                          $$.agregarHijo(new Nodo_Arbol("--",""));}
    | ASIGNACION                                        { $$ = new Nodo_Arbol("RFOR","");
                                                            $$.agregarHijo($1);}
;

expresion
    : expresion MAS expresion               { $$ = new Nodo_Arbol("Ex","");
                                                $$.agregarHijo($1);
                                                $$.agregarHijo(new Nodo_Arbol("+",""));
                                                $$.agregarHijo($3);
                                                $$.tipo= $1.tipo + $3.tipo; }
    | expresion MENOS expresion             { $$ = new Nodo_Arbol("Ex","");
                                                $$.agregarHijo($1);
                                                $$.agregarHijo(new Nodo_Arbol("-",""));
                                                $$.agregarHijo($3);
                                                $$.tipo= $1.tipo - $3.tipo; }
    | expresion POR expresion               { $$ = new Nodo_Arbol("Ex","");
                                                $$.agregarHijo($1);
                                                $$.agregarHijo(new Nodo_Arbol("*",""));
                                                $$.agregarHijo($3);
                                                $$.tipo= $1.tipo * $3.tipo; }
    | expresion DIVIDIDO expresion          { $$ = new Nodo_Arbol("Ex","");
                                                $$.agregarHijo($1);
                                                $$.agregarHijo(new Nodo_Arbol("/",""));
                                                $$.agregarHijo($3);
                                                $$.tipo= $1.tipo / $3.tipo; }
    | expresion POTENCIA expresion          { $$ = new Nodo_Arbol("Ex","");
                                                $$.agregarHijo($1);
                                                $$.agregarHijo(new Nodo_Arbol("^",""));
                                                $$.agregarHijo($3);
                                                $$.tipo= $1.tipo ** $3.tipo; }
    | expresion MOD expresion               { $$ = new Nodo_Arbol("Ex","");
                                                $$.agregarHijo($1);
                                                $$.agregarHijo(new Nodo_Arbol("%",""));
                                                $$.agregarHijo($3);
                                                $$.tipo= $1.tipo % $3.tipo; }
    | expresion DOBLEIGUAL expresion        { $$ = new Nodo_Arbol("Ex","");
                                                $$.agregarHijo($1);
                                                $$.agregarHijo(new Nodo_Arbol("==",""));
                                                $$.agregarHijo($3); }
    | expresion NOIGUAL expresion           { $$ = new Nodo_Arbol("Ex","");
                                                $$.agregarHijo($1);
                                                $$.agregarHijo(new Nodo_Arbol("!=",""));
                                                $$.agregarHijo($3);}
    | expresion MENQUE expresion            { $$ = new Nodo_Arbol("Ex","");
                                                $$.agregarHijo($1);
                                                $$.agregarHijo(new Nodo_Arbol("<",""));
                                                $$.agregarHijo($3); }
    | expresion MAYQUE expresion            { $$ = new Nodo_Arbol("Ex","");
                                                $$.agregarHijo($1);
                                                $$.agregarHijo(new Nodo_Arbol(">",""));
                                                $$.agregarHijo($3); }
    | expresion MENIGQUE expresion          { $$ = new Nodo_Arbol("Ex","");
                                                $$.agregarHijo($1);
                                                $$.agregarHijo(new Nodo_Arbol("<=",""));
                                                $$.agregarHijo($3); }
    | expresion MAYIGQUE expresion          { $$ = new Nodo_Arbol("Ex","");
                                                $$.agregarHijo($1);
                                                $$.agregarHijo(new Nodo_Arbol(">=",""));
                                                $$.agregarHijo($3); }
    | expresion AND expresion               { $$ = new Nodo_Arbol("Ex","");
                                                $$.agregarHijo($1);
                                                $$.agregarHijo(new Nodo_Arbol("&&",""));
                                                $$.agregarHijo($3); }
    | expresion NOT expresion               { $$ = new Nodo_Arbol("Ex","");
                                                $$.agregarHijo($1);
                                                $$.agregarHijo(new Nodo_Arbol("!",""));
                                                $$.agregarHijo($3); }
    | expresion OR expresion                { $$ = new Nodo_Arbol("Ex","");
                                                $$.agregarHijo($1);
                                                $$.agregarHijo(new Nodo_Arbol("||",""));
                                                $$.agregarHijo($3); }
    | PARIZQ expresion PARDER               {$$ = new Nodo_Arbol("Ex", "");
                                                 $$.agregarHijo($2); }
    | LOWER PARIZQ expresion PARDER         { $$ = new Nodo_Arbol("Low", "");
                                                 $$.agregarHijo($3);
                                                 $$.tipo= $3.tipo.toLowerCase();  }
    | UPPER PARIZQ expresion PARDER         { $$ = new Nodo_Arbol("Upper", "");
                                                 $$.agregarHijo($3);
                                                 $$.tipo= $3.tipo.toUpperCase();}
    | LENGTH PARIZQ expresion PARDER         { $$ = new Nodo_Arbol("Ex", "");
                                                 $$.agregarHijo($3); }
    | TRUNCATE PARIZQ expresion PARDER         { $$ = new Nodo_Arbol("Ex", "");
                                                 $$.agregarHijo($3); }
    | ROUND PARIZQ expresion PARDER         { $$ = new Nodo_Arbol("Ex", "");
                                                 $$.agregarHijo($3);  }
    | TYPEOF PARIZQ expresion PARDER         { $$ = new Nodo_Arbol("Ex", "");
                                                 $$.agregarHijo($3); }
    | TOSTRING PARIZQ expresion PARDER         { $$ = new Nodo_Arbol("Ex", "");
                                                 $$.agregarHijo($3); }
    | NOT expresion                         { $$ = new Nodo_Arbol("Ex", "");
                                                 $$.agregarHijo($2); }
    | MENOS expresion %prec UMENOS          { $$ = new Nodo_Arbol("Ex", "");
                                                 $$.agregarHijo($2); }
    | TRUE                                  { $$ = new Nodo_Arbol("Ex","");
                                                $$.agregarHijo(new Nodo_Arbol($1,"")); }
    | FALSE                                 { $$ = new Nodo_Arbol("Ex","");
                                                $$.agregarHijo(new Nodo_Arbol($1,"")); }
    | CADENA                                { $$ = new Nodo_Arbol("Ex","");
												$1=$1.slice(1, -1);												
                                                $$.agregarHijo(new Nodo_Arbol($1,""));
                                                $$.tipo=$1; }
    | CARACTER                              { $$ = new Nodo_Arbol("Ex","");
                                                $1=$1.slice(1, -1);
                                                $$.agregarHijo(new Nodo_Arbol($1,""));
                                                $$.tipo=$1; }
    | DECIMAL                               { $$ = new Nodo_Arbol("Ex","");
                                                $$.tipo=Number($1);
                                                $$.agregarHijo(new Nodo_Arbol($1,"")); }
    | ENTERO                                { $$ = new Nodo_Arbol("Ex","");
                                                $$.tipo=Number($1);
                                                $$.agregarHijo(new Nodo_Arbol($1,"")); }
    | IDENTIFICADOR                         { $$ = new Nodo_Arbol("Ex","");
                                                $$.agregarHijo(new Nodo_Arbol($1,"")); }
    | IDENTIFICADOR PARIZQ PARDER           { $$ = new Nodo_Arbol("Ex","");
                                                $$.agregarHijo(new Nodo_Arbol($1,"")); }
    | IDENTIFICADOR PARIZQ L_exp PARDER     { $$ = new Nodo_Arbol("Ex","");
                                                $$.agregarHijo(new Nodo_Arbol($1,""));
                                                $$.agregarHijo($3); }        
;
L_exp
    : L_exp COMA expresion              { $$= new Nodo_Arbol("LEXPRESION","");
                                            $$.agregarHijo($1);
                                            $$.agregarHijo($3); }
    | expresion                         { $$= new Nodo_Arbol("LEXPRESION","");
                                          $$.agregarHijo($1); }
;