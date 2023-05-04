
%{
	const Logical = require('./clases/Logical');
	const Relational = require('./clases/Relational');
    const Arithmetical = require('./clases/Arithmetical');
    const Value = require('./clases/Value');
    const Print = require('./clases/Print');
    const Declaration = require('./clases/Declaration');
    const IfList = require('./clases/IfList');
    const Else = require('./clases/Else');
    const If = require('./clases/If');
    const While = require('./clases/While');
    const DoWhile = require('./clases/DoWhile');
    const Assignment = require('./clases/Assignment');
    const For = require('./clases/For');
    const Unary = require('./clases/Unary');
	const Type = require('./clases/Type');
    const Count = require('./clases/Counters');
    const SymbolTable = require('./clases/SymbolTable');
	const Call = require('./clases/Call');
    const Function = require('./clases/Function');
    global_var = []
    var count = new Count();
    var symbolt = new SymbolTable(null);
    symbolt.count = count;
    symbolt.functions = [];
%}

%lex 

%options case-insensitive

%%

\"((\\\\")|[^\n\"])*\"      {yytext= yytext.substr(1,yyleng-2); return 'CADENA';}
\s+                 // se ignoran espacios en blanco

"//".*                  // comentario simple
[/][*][^*]*[*]+([^/*][^*]*[*]+)*[/]     //comentario multiple
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
\'((\\\\')|[^\n\'])\'      {yytext= yytext.substr(1,yyleng-2); return 'CARACTER';}
[0-9]+("."[0-9]+)\b         return 'DECIMAL';
[0-9]+\b                    return 'ENTERO';
([a-zA-Z])[a-zA-Z0-9_]*     return 'IDENTIFICADOR';
<<EOF>>                     return 'EOF';
.                           {console.log('Este es un error lexico'+yytext+yylloc.first_line+yylloc.first_column)}
/lex

%left 'OR'
%left 'AND'
%right 'NOT'
%left  'DOBLEIGUAL' 'NOIGUAL' 'MENQUE' 'MAYQUE' 'MENIGQUE' 'MAYIGQUE'
%left   'MAS' 'MENOS'
%left   'POR' 'DIVIDIDO' 'MOD'
%nonassoc   'POTENCIA'
%right UMENOS


%start ini

%% /* Definición de la gramática */

ini
	: instrucciones EOF {
		// cuado se haya reconocido la entrada completa retornamos el AST
		return $1;
	}
;

instrucciones
	: instrucciones instruccion 	{ $1.push($2); $$ = $1; }
	| instruccion					{ $$ = [$1]; }
;

instruccion
	: RIMPRIMIR PARIZQ expresion_cadena PARDER PTCOMA	{ $$ = instruccionesAPI.nuevoImprimir($3); }
	| RMIENTRAS PARIZQ expresion_logica PARDER LLAVIZQ instrucciones LLAVDER
														{ $$ = instruccionesAPI.nuevoMientras($3, $6); }
	| RPARA PARIZQ IDENTIFICADOR IGUAL expresion_numerica PTCOMA expresion_logica PTCOMA IDENTIFICADOR MAS MAS PARDER LLAVIZQ instrucciones LLAVDER
														{ $$ = instruccionesAPI.nuevoPara($3,$5,$7,$9,$14) }
	| RNUMERO IDENTIFICADOR PTCOMA						{ $$ = instruccionesAPI.nuevoDeclaracion($2, TIPO_DATO.NUMERO); }
	| RSTRING IDENTIFICADOR PTCOMA						{ $$ = instruccionesAPI.nuevoDeclaracion($2, TIPO_DATO.STRING); }
	| IDENTIFICADOR IGUAL expresion_cadena PTCOMA		{ $$ = instruccionesAPI.nuevoAsignacion($1, $3); } //esto soporta expresiones_cadena y expresion_numerica

	| RIF PARIZQ expresion_logica PARDER LLAVIZQ instrucciones LLAVDER
														{ $$ = instruccionesAPI.nuevoIf($3, $6); }
	| RIF PARIZQ expresion_logica PARDER LLAVIZQ instrucciones LLAVDER RELSE LLAVIZQ instrucciones LLAVDER
														{ $$ = instruccionesAPI.nuevoIf($3, $6, $10); }

	| RSWITCH PARIZQ expresion_numerica PARDER LLAVIZQ casos LLAVDER
		{ $$ = instruccionesAPI.nuevoSwitch($3,$6);}
	| IDENTIFICADOR operadores expresion_numerica PTCOMA	
	                                                    { $$ = instruccionesAPI.nuevoAsignacionSimplificada($1, $2, $3); }
	| error { console.error('Este es un error sintáctico: ' + yytext + ', en la linea: ' + this._$.first_line + ', en la columna: ' + this._$.first_column); }
;

casos : casos caso_evaluar
    {
      $1.push($2);
	  $$ = $1;
    }
  | caso_evaluar
  	{ $$ = instruccionesAPI.nuevoListaCasos($1);}
;

caso_evaluar : RCASE expresion_numerica DOSPTS instrucciones
    { $$ = instruccionesAPI.nuevoCaso($2,$4); }
  | RDEFAULT DOSPTS instrucciones
    { $$ = instruccionesAPI.nuevoCasoDef($3); }
;

operadores
    : O_MAS      { $$ = instruccionesAPI.nuevoOperador(TIPO_OPERACION.SUMA); }
	| O_MENOS    { $$ = instruccionesAPI.nuevoOperador(TIPO_OPERACION.RESTA); }
    | O_POR      { $$ = instruccionesAPI.nuevoOperador(TIPO_OPERACION.MULTIPLICACION); }
	| O_DIVIDIDO { $$ = instruccionesAPI.nuevoOperador(TIPO_OPERACION.DIVISION); }
;


expresion_numerica
	: MENOS expresion_numerica %prec UMENOS				{ $$ = instruccionesAPI.nuevoOperacionUnaria($2, TIPO_OPERACION.NEGATIVO); }
	| expresion_numerica MAS expresion_numerica			{ $$ = instruccionesAPI.nuevoOperacionBinaria($1, $3, TIPO_OPERACION.SUMA); }
	| expresion_numerica MENOS expresion_numerica		{ $$ = instruccionesAPI.nuevoOperacionBinaria($1, $3, TIPO_OPERACION.RESTA); }
	| expresion_numerica POR expresion_numerica			{ $$ = instruccionesAPI.nuevoOperacionBinaria($1, $3, TIPO_OPERACION.MULTIPLICACION); }
	| expresion_numerica DIVIDIDO expresion_numerica	{ $$ = instruccionesAPI.nuevoOperacionBinaria($1, $3, TIPO_OPERACION.DIVISION); }
	| PARIZQ expresion_numerica PARDER					{ $$ = $2; }
	| ENTERO											{ $$ = instruccionesAPI.nuevoValor(Number($1), TIPO_VALOR.NUMERO); }
	| DECIMAL											{ $$ = instruccionesAPI.nuevoValor(Number($1), TIPO_VALOR.NUMERO); }
	| IDENTIFICADOR										{ $$ = instruccionesAPI.nuevoValor($1, TIPO_VALOR.IDENTIFICADOR); }
;

expresion_cadena
	: expresion_cadena CONCAT expresion_cadena			{ $$ = instruccionesAPI.nuevoOperacionBinaria($1, $3, TIPO_OPERACION.CONCATENACION); }
	| CADENA											{ $$ = instruccionesAPI.nuevoValor($1, TIPO_VALOR.CADENA); }
	| expresion_numerica								{ $$ = $1; }
;

expresion_relacional
	: expresion_numerica MAYQUE expresion_numerica		{ $$ = instruccionesAPI.nuevoOperacionBinaria($1, $3, TIPO_OPERACION.MAYOR_QUE); }
	| expresion_numerica MENQUE expresion_numerica		{ $$ = instruccionesAPI.nuevoOperacionBinaria($1, $3, TIPO_OPERACION.MENOR_QUE); }
	| expresion_numerica MAYIGQUE expresion_numerica	{ $$ = instruccionesAPI.nuevoOperacionBinaria($1, $3, TIPO_OPERACION.MAYOR_IGUAL); }
	| expresion_numerica MENIGQUE expresion_numerica	{ $$ = instruccionesAPI.nuevoOperacionBinaria($1, $3, TIPO_OPERACION.MENOR_IGUAL); }
	| expresion_cadena DOBLEIG expresion_cadena			{ $$ = instruccionesAPI.nuevoOperacionBinaria($1, $3, TIPO_OPERACION.DOBLE_IGUAL); }
	| expresion_cadena NOIG expresion_cadena			{ $$ = instruccionesAPI.nuevoOperacionBinaria($1, $3, TIPO_OPERACION.NO_IGUAL); }
;

expresion_logica
	: expresion_relacional AND expresion_relacional     { $$ = instruccionesAPI.nuevoOperacionBinaria($1, $3, TIPO_OPERACION.AND); }
	| expresion_relacional OR expresion_relacional 		{ $$ = instruccionesAPI.nuevoOperacionBinaria($1, $3, TIPO_OPERACION.OR); }
	| NOT expresion_relacional							{ $$ = instruccionesAPI.nuevoOperacionUnaria($2, TIPO_OPERACION.NOT); }
	| expresion_relacional								{ $$ = $1; }
;