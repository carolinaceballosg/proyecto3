module AST

data Programa = modulo(list[Declaration] decls, MainMethod principal);
data Declaration
  = objeto(ObjectCreation objectCreation)
  | asignMetodo(MethodAssignment methodAssignment)
  ;
data MainMethod = main(Block block);
data ObjectCreation = objeto(
    str name,
    list[GenericParams] generics,
    list[Heritage] heritage,
    list[ObjectParams] params,
    list[ObjectBody] body
);
data Heritage = herencia(str name);
data GenericParams = genericos(list[GenericSingleParam] params);
data GenericSingleParam = generico(str modifier, str name);
data ObjectParams = params(list[Param] params);
data Param = param(str modifier, str name, Type tipo);
data ObjectBody = cuerpo(list[ObjectContent] content);
data ObjectContent
  = atributo(Attribute attribute)
  | metodo(MethodDecl methodDecl)
  ;
data Attribute = attr(str modifier, str name, list[Type] tipo, Expression valorExpr);
data Type
  = tipo(str name, list[TypeList] typeList)
  | funcion(str name, list[TypeList] typeList, Type returnType)
  ;
data TypeList = tipos(list[Type] types);
data MethodDecl = metodo(
    bool override,
    str name,
    list[Param] params,
    list[Type] returnType,
    MethodBody body
);
data MethodBody
  = cuerpo(Block block)
  | expresion(Expression expression)
  ;
data MethodAssignment = asigna(
    MethodPath path,
    list[Param] params,
    list[Type] returnType,
    list[Block] body
);
data MethodPath
  = rutaId(list[str] identifiers)
  | rutaClase(str clase, list[str] identifiers)
  ;
data PathName = ruta(list[str] identifiers);
data Block = bloque(list[Statement] statements);
data Statement
  = valDecl(ValueDecl valueDecl)
  | varDecl(VarDecl varDecl)
  | asignacion(Assignment assignment)
  | bucle(ForStatement forStatement)
  | imprimir(PrintStatement printStatement)
  | condicional(IfExpr ifExpr)
  | llamadaStmt(CallStmt callStmt)
  | nuevoStmt(NewExpr newExpr, list[StmtSuffix] sufijos)
  | nuevoSimpleStmt(Expression newSimple)
  | refStmt(PathName path)
  ;
data CallStmt = stmtLlamada(PathName path, list[Expression] args, list[StmtSuffix] sufijos);
data StmtSuffix
  = sufLlamada(list[Expression] args)
  | sufAcceso(str name)
  ;
data ValueDecl = valor(str name, list[Type] tipo, Expression valorExpr);
data VarDecl = variable(str name, list[Type] tipo, Expression valorExpr);
data Assignment = asigna(PathName path, Expression valorExpr);
data ForStatement = para(ForClausula clausula, Statement body);
data ForClausula
  = flecha(ForVariables vars, Expression source)
  | enLista(str identifier, Expression collection)
  ;
data ForVariables
  = una(str identifier)
  | varias(list[str] identifiers)
  ;
data PrintStatement = imprime(str tipo, Expression valorExpr);
data IfExpr = si(Expression condition, IfBranch thenBranch, list[IfBranch] elseBranch);
data IfBranch
  = ramaExpr(Expression expression)
  | ramaPrint(PrintStatement printStatement)
  ;

data Expression
  = siValor(Expression condition, Expression thenExpr, Expression elseExpr)
  | operation(Expression izq, Expression der)
  | y(Expression izq, Expression der)
  | no(Expression atomic)
  | menor(Expression izq, Expression der)
  | mayor(Expression izq, Expression der)
  | menorIgual(Expression izq, Expression der)
  | mayorIgual(Expression izq, Expression der)
  | igual(Expression izq, Expression der)
  | distinto(Expression izq, Expression der)
  | suma(Expression izq, Expression der)
  | resta(Expression izq, Expression der)
  | mul(Expression izq, Expression der)
  | div(Expression izq, Expression der)
  | modulo(Expression izq, Expression der)
  | pot(Expression base, Expression exponent)
  | negativo(Expression expr)
  | acceso(Expression postFix, str name)
  | llamada(Expression postFix, list[Expression] args)
  | literal(Literal literal)
  | variable(str name)
  | nuevo(NewExpr newExpr)
  | nuevoSimple(str name, list[TypeList] typeList, list[ObjectBody] body)
  | paren(Expression expr)
  ;
data NewExpr = nuevo(
    str name,
    list[TypeList] typeList,
    list[Expression] args,
    list[ObjectBody] body
);
data Literal
  = entero(int valor)
  | booleano(bool valorBool)
  | cadena(str cadena)
  ;