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
        | instruccion                { $$= new Nodo_Arbol("INSTRUCCIONES","");
                                        $$.agregarHijo($1);  }
;
instruccion 
    : PRINT PARIZQ expresion PARDER PTCOMA                  { $$ = new Nodo_Arbol("PRINT","");                                                    
                                                                $$.agregarHijo($3);																
																var texto= $$.recorrer_print($3);
                                                                texto = texto.replace(/Ex/g, "");
												document.getElementById("txtsalida1"+publico_id).value+=" Print: "+ texto+"\n"; }
    | error  { document.getElementById("txtsalida1"+publico_id).value+="Error sintactico en la Linea: " + this._$.first_line + " en la Columna: " + this._$.first_column+"\n";
    //Salida.push('Este es un error Sintactico: ' + yytext + ', en la linea: ' + this.$.first_line + ', en la columna: ' + this.$.first_line.first_column);
                    //errores.push(new Error_(this.$.first_line,this.$.first_column,"Sintactico","No se esperaba la expresion: " + yytext));
                    //console.log('Este es un error Sintactico: ' + yytext + ', en la linea: ' + yylineno.first_line + ', en la columna: ' + yylineno.first_column);
                    }
;


expresion
    : expresion MAS expresion               { $$ = new Nodo_Arbol("Ex","");
                                                $$.agregarHijo($1);
                                                $$.agregarHijo(new Nodo_Arbol("+","suma"));
                                                $$.agregarHijo($3); }
    | expresion MENOS expresion             { $$ = new Nodo_Arbol("Ex","");
                                                $$.agregarHijo($1);
                                                $$.agregarHijo(new Nodo_Arbol("-","resta"));
                                                $$.agregarHijo($3); }
    | expresion POR expresion               { $$ = new Nodo_Arbol("Ex","");
                                                $$.agregarHijo($1);
                                                $$.agregarHijo(new Nodo_Arbol("*","multiplicar"));
                                                $$.agregarHijo($3); }
    | expresion DIVIDIDO expresion          { $$ = new Nodo_Arbol("Ex","");
                                                $$.agregarHijo($1);
                                                $$.agregarHijo(new Nodo_Arbol("/","division"));
                                                $$.agregarHijo($3); }
    | expresion POTENCIA expresion          { $$ = new Nodo_Arbol("Ex","");
                                                $$.agregarHijo($1);
                                                $$.agregarHijo(new Nodo_Arbol("^","potencia"));
                                                $$.agregarHijo($3); }
    | expresion MOD expresion               { $$ = new Nodo_Arbol("Ex","");
                                                $$.agregarHijo($1);
                                                $$.agregarHijo(new Nodo_Arbol("%","mod"));
                                                $$.agregarHijo($3); }
    | expresion DOBLEIGUAL expresion        { $$ = new Nodo_Arbol("Ex","");
                                                $$.agregarHijo($1);
                                                $$.agregarHijo(new Nodo_Arbol("==","dobleIgual"));
                                                $$.agregarHijo($3); }
    | expresion NOIGUAL expresion           { $$ = new Nodo_Arbol("Ex","");
                                                $$.agregarHijo($1);
                                                $$.agregarHijo(new Nodo_Arbol("!=","noIgual"));
                                                $$.agregarHijo($3);}
    | expresion MENQUE expresion            { $$ = new Nodo_Arbol("Ex","");
                                                $$.agregarHijo($1);
                                                $$.agregarHijo(new Nodo_Arbol("<","menorQue"));
                                                $$.agregarHijo($3); }
    | expresion MAYQUE expresion            { $$ = new Nodo_Arbol("Ex","");
                                                $$.agregarHijo($1);
                                                $$.agregarHijo(new Nodo_Arbol(">","mayorQue"));
                                                $$.agregarHijo($3); }
    | expresion MENIGQUE expresion          { $$ = new Nodo_Arbol("Ex","");
                                                $$.agregarHijo($1);
                                                $$.agregarHijo(new Nodo_Arbol("<=","menorIgualQue"));
                                                $$.agregarHijo($3); }
    | expresion MAYIGQUE expresion          { $$ = new Nodo_Arbol("Ex","");
                                                $$.agregarHijo($1);
                                                $$.agregarHijo(new Nodo_Arbol(">=","mayorIgualQue"));
                                                $$.agregarHijo($3); }
    | expresion AND expresion               { $$ = new Nodo_Arbol("Ex","");
                                                $$.agregarHijo($1);
                                                $$.agregarHijo(new Nodo_Arbol("&&","and"));
                                                $$.agregarHijo($3); }
    | expresion NOT expresion               { $$ = new Nodo_Arbol("Ex","");
                                                $$.agregarHijo($1);
                                                $$.agregarHijo(new Nodo_Arbol("!","not"));
                                                $$.agregarHijo($3); }
    | expresion OR expresion                { $$ = new Nodo_Arbol("Ex","");
                                                $$.agregarHijo($1);
                                                $$.agregarHijo(new Nodo_Arbol("||","or"));
                                                $$.agregarHijo($3); }
    | PARIZQ expresion PARDER               {$$ = new Nodo_Arbol("Ex", "");
                                                 $$.agregarHijo($2); }
    | LOWER PARIZQ expresion PARDER         { $$ = new Nodo_Arbol("Ex", "");
                                                 $$.agregarHijo($3);  }
    | UPPER PARIZQ expresion PARDER         { $$ = new Nodo_Arbol("Ex", "");
                                                 $$.agregarHijo($3); }
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
                                                $$.agregarHijo(new Nodo_Arbol($1,"true")); }
    | FALSE                                 { $$ = new Nodo_Arbol("Ex","");
                                                $$.agregarHijo(new Nodo_Arbol($1,"false")); }
    | CADENA                                { $$ = new Nodo_Arbol("Ex","");
												$1=$1.slice(1, -1);												
                                                $$.agregarHijo(new Nodo_Arbol($1,"cadena")); }
    | CARACTER                              { $$ = new Nodo_Arbol("Ex","");
                                                $$.agregarHijo(new Nodo_Arbol($1,"caracter")); }
    | DECIMAL                               { $$ = new Nodo_Arbol("Ex","");
                                                $$.agregarHijo(new Nodo_Arbol($1,"decimal")); }
    | ENTERO                                { $$ = new Nodo_Arbol("Ex","");
                                                $$.agregarHijo(new Nodo_Arbol($1,"entero")); }
    | IDENTIFICADOR                         { $$ = new Nodo_Arbol("Ex","");
                                                $$.agregarHijo(new Nodo_Arbol($1,"identificador")); }
    | IDENTIFICADOR PARIZQ PARDER           { $$ = new Nodo_Arbol("Ex","");
                                                $$.agregarHijo(new Nodo_Arbol($1,"identificador")); }
    | IDENTIFICADOR PARIZQ L_exp PARDER     { $$ = new Nodo_Arbol("Ex","");
                                                $$.agregarHijo(new Nodo_Arbol($1,"identificador"));
                                                $$.agregarHijo($3); }        
;
L_exp
    : L_exp COMA expresion              { $$= new Nodo_Arbol("LEXPRESION","");
                                            $$.agregarHijo($1);
                                            $$.agregarHijo($3); }
    | expresion                         { $$= new Nodo_Arbol("LEXPRESION","");
                                          $$.agregarHijo($1); }
;