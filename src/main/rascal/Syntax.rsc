module Syntax

layout Layout = LayoutElem* !>> [\ \t\n\r] !>> "//";

lexical LayoutElem = [\ \t\n\r] | Comment;
lexical Comment = @category="comment" "//" ![\n]* !>> ![\n];

keyword Reserved =
 "object" | "extends"| "method"| "val"| "var"| "new"| "override"
 | "if"| "then"| "else"| "for"| "in"| "print"| "println"| "main"
 | "and"| "or"| "neg"| "true"| "false";

lexical Identifier = @category="variable" ([a-z][a-zA-Z0-9_]* !>> [a-zA-Z0-9_]) \ Reserved;
lexical ClassName = @category="Type" ([A-Z][a-zA-Z0-9]* !>> [a-zA-Z0-9]);
lexical IntLiteral = @category="number" [0-9]+ !>> [0-9];
lexical StringLiteral = "\"" StringPart* "\"";
lexical StringPart
  = texto: StringText
  | interpolation: Interpolation
  ;

lexical StringText = @category="string" ![\"$]+ !>> ![\"$];
lexical Interpolation
  = simple: "$" Identifier
  | compleja: "$" "{" [\ \t\n\r]* !>> [\ \t\n\r] Expression [\ \t\n\r]* "}"
  ;

start syntax Module
  = modulo: Declaration* decls MainMethod principal
  ;

syntax Declaration
  = objeto: ObjectCreation
  | asignMetodo: MethodAssignment
  ;

syntax MainMethod
  = main: "method" "main" Block
  ;

syntax ObjectCreation
  = objeto: "object" ClassName GenericParams? Heritage? ObjectParams? ObjectBody?
  ;

syntax Heritage
  = herencia: "extends" ClassName
  ;

syntax GenericParams
  = genericos: "[" {GenericSingleParam ","}+ "]"
  ;

syntax GenericSingleParam
  = generico: ("+" | "-")? (Identifier | ClassName)
  ;

syntax ObjectParams
  = params: "(" {Param ","}* ")"
  ;

syntax Param
  = param: ("val" | "var")? Identifier ":" Type
  ;

syntax ObjectBody
  = cuerpo: "{" ObjectContent* "}"
  ;

syntax ObjectContent
  = atributo: Attribute
  | metodo: MethodDecl
  ;

syntax Attribute
  = attr: ("val" | "var") Identifier (":" Type)? "=" Expression
  ;

syntax Type
  = tipo: ClassName TypeList?
  | funcion: ClassName TypeList? "-\>" Type
  ;

syntax TypeList
  = tipos: "[" {Type ","}+ "]"
  ;

syntax MethodDecl
  = metodo: "override"? Identifier "(" {Param ","}* ")" (":" Type)? "=" MethodBody
  ;

syntax MethodBody
  = cuerpo: Block
  | expresion: Expression
  ;

syntax MethodAssignment
  = asigna: MethodPath "=" "method" "(" {Param ","}* ")" (":" Type)? Block?
  ;

syntax MethodPath
  = rutaId: {Identifier "."}+
  | rutaClase: ClassName "." {Identifier "."}+
  ;

syntax PathName
  = ruta: {Identifier "."}+
  ;
syntax Block
  = bloque: "{" Statement* "}"
  ;

syntax Statement
  = valDecl: ValueDecl
  | varDecl: VarDecl
  | asignacion: Assignment
  | bucle: ForStatement
  | imprimir: PrintStatement
  | condicional: IfExpr        
  | llamadaStmt: CallStmt
  | nuevoStmt: NewExpr StmtSuffix*
  | nuevoSimpleStmt: NewSimple
  | refStmt: PathName
  ;

syntax CallStmt
  = stmtLlamada: PathName "(" {Expression ","}* ")" StmtSuffix*
  ;

syntax StmtSuffix
  = sufLlamada: "(" {Expression ","}* ")"
  | sufAcceso: "." Identifier
  ;

syntax ValueDecl
  = valor: "val" Identifier (":" Type)? "=" Expression
  ;

syntax VarDecl
  = variable: "var" Identifier (":" Type)? "=" Expression
  ;

syntax Assignment
  = asigna: PathName "=" Expression
  ;

syntax ForStatement
  = para: "for" "(" ForClausula ")" Statement
  ;

syntax ForClausula
  = flecha: ForVariables "\<-" Expression 
  | enLista: Identifier "in" Expression
  ;

syntax ForVariables
  = una: Identifier
  | varias: "(" {Identifier ","}+ ")"
  ;

syntax PrintStatement
  = imprime: ("print" | "println") "(" Expression ")"
  ;
syntax IfExpr
  = si: "if" BoolExpr "then" IfBranch ("else" IfBranch)?
  ;

syntax IfBranch
  = ramaExpr: Expression
  | ramaPrint: PrintStatement
  ;

syntax BoolExpr
  = operation: BoolExpr "or" BoolAnd
  | BoolAnd
  ;

syntax BoolAnd
  = y: BoolAnd "and" BoolAtomico
  | BoolAtomico
  ;

syntax BoolAtomico
  = no: "neg" !>> [a-zA-Z0-9_] BoolAtomico
  | Comparison
  ;

syntax Comparison
  = menor: SumExpr "\<" SumExpr
  | mayor: SumExpr "\>" SumExpr
  | menorIgual: SumExpr "\<=" SumExpr
  | mayorIgual: SumExpr "\>=" SumExpr
  | igual: SumExpr "=" SumExpr
  | distinto: SumExpr "\<\>" SumExpr
  | SumExpr
  ;

syntax Expression
  = siValor: "if" BoolExpr "then" Expression "else" Expression
  | BoolExpr
  ;

syntax SumExpr
  = suma: SumExpr "+" MulExpr
  | resta: SumExpr "-" MulExpr
  | MulExpr
  ;

syntax MulExpr
  = mul: MulExpr "*" PowExpr
  | div: MulExpr "/" PowExpr
  | modulo: MulExpr "%" PowExpr
  | PowExpr
  ;

syntax PowExpr
  = pot: UnaryExpr "**" PowExpr
  | UnaryExpr
  ;

syntax UnaryExpr
  = negativo: "-" UnaryExpr
  | PostFix
  | NewSimple
  ;

syntax PostFix
  = acceso: PostFix "." Identifier
  | llamada: PostFix "(" {Expression ","}* ")"
  | Primary
  ;

syntax Primary
  = literal: Literal
  | variable: Identifier
  | nuevo: NewExpr
  | paren: "(" Expression ")"
  ;

syntax NewExpr
  = nuevo: "new" ClassName TypeList? "(" {Expression ","}* ")" ObjectBody?
  ;

syntax NewSimple
  = nuevoSimple: "new" ClassName TypeList? ObjectBody?
  ;

syntax Literal
  = entero: IntLiteral
  | booleano: BoolLiteral
  | cadena: StringLiteral
  ;

lexical BoolLiteral
  = "true"
  | "false"
  ;