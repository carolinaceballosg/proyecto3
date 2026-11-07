module Checker

// Verificador de tipos de Objectilang con TypePal (basado en la seccion 9 del tutorial).
// Trabaja sobre la sintaxis concreta: importa Syntax, NO importa AST.

extend analysis::typepal::TypePal;
import Syntax;
import ParseTree;

// ---------- Tipos y roles ----------

data AType
  = tipoInt()
  | tipoBool()
  | tipoString()
  | tipoChar()
  | tipoLista(AType elemento)
  | tipoClase(str nombre)
  | tipoGenerico(str nombre)
  | tipoDesconocido()        // lo que todavia no verificamos (llamadas a metodos, accesos con ".")
  ;

data IdRole
  = rolClase()
  | rolVal()
  | rolVar()
  | rolParam()
  | rolMetodo()
  | rolGenerico()
  ;

// Como se muestran los tipos en los mensajes de error
str prettyAType(tipoInt()) = "Int";
str prettyAType(tipoBool()) = "Boolean";
str prettyAType(tipoString()) = "String";
str prettyAType(tipoChar()) = "Char";
str prettyAType(tipoLista(AType e)) = "List[<prettyAType(e)>]";
str prettyAType(tipoClase(str n)) = n;
str prettyAType(tipoGenerico(str n)) = n;
str prettyAType(tipoDesconocido()) = "?";

// Tipos basicos del lenguaje (se aceptan Int e Integer)
map[str, AType] basicos = (
  "Int": tipoInt(), "Integer": tipoInt(),
  "Boolean": tipoBool(), "Bool": tipoBool(),
  "String": tipoString(),
  "Char": tipoChar()
);

set[IdRole] rolesValor = {rolVal(), rolVar(), rolParam(), rolMetodo()};

// ---------- Subtipos (incluye herencia con extends) ----------

map[str, str] padres = ();   // clase -> clase padre; se llena antes de verificar

map[str, str] calcularPadres(Tree pt)
  = ("<n>" : "<p>"
    | /(ObjectCreation) `object <ClassName n> <GenericParams? g> extends <ClassName p> <ObjectParams? ps> <ObjectBody? b>` := pt);

bool esSubclase(str hija, str madre) {
  int pasos = 0;
  while (hija != madre) {
    if (hija notin padres || pasos > 100) return false;
    hija = padres[hija];
    pasos += 1;
  }
  return true;
}

bool esSubtipo(AType a, AType b) {
  if (a == b) return true;
  if (tipoDesconocido() := a || tipoDesconocido() := b) return true;
  if (tipoGenerico(_) := a || tipoGenerico(_) := b) return true;
  if (tipoLista(x) := a, tipoLista(y) := b) return esSubtipo(x, y);
  if (tipoClase(x) := a, tipoClase(y) := b) return esSubclase(x, y);
  return false;
}

// ---------- Ayudas ----------

bool omitir(appl(prod(lit(_), _, _), _)) = true;
bool omitir(appl(prod(layouts(_), _, _), _)) = true;
default bool omitir(Tree _) = false;

// Recolecta todos los hijos de un nodo (sin literales ni espacios)
void collectHijos(Tree t, Collector c) {
  if (appl(_, list[Tree] hijos) := t) {
    for (Tree h <- hijos, !omitir(h)) collect(h, c);
  }
}

// Paso directo: el nodo tiene el mismo tipo que su unico hijo
void pasar(Tree current, Tree hijo, Collector c) {
  c.fact(current, hijo);
  collect(hijo, c);
}

// ---------- Programa, bloques y scopes ----------

void collect(current: (Module) `<Declaration* ds> <MainMethod m>`, Collector c) {
  c.enterScope(current);
    collect(ds, m, c);
  c.leaveScope(current);
}

void collect(current: (Block) `{ <Statement* ss> }`, Collector c) {
  c.enterScope(current);
    collect(ss, c);
  c.leaveScope(current);
}

// ---------- Objetos ----------

void collect(current: (ObjectCreation) `object <ClassName n> <GenericParams? gs> <Heritage? h> <ObjectParams? ps> <ObjectBody? b>`, Collector c) {
  c.define("<n>", rolClase(), n, defType(tipoClase("<n>")));
  c.enterScope(current);
    collect(gs, h, ps, b, c);
  c.leaveScope(current);
}

// Regla: la clase padre debe existir
void collect(current: (Heritage) `extends <ClassName p>`, Collector c) {
  c.use(p, {rolClase()});
}

void collect(GenericSingleParam current, Collector c) {
  for (/ClassName n := current) c.define("<n>", rolGenerico(), n, defType(tipoGenerico("<n>")));
  for (/Identifier n := current) c.define("<n>", rolGenerico(), n, defType(tipoGenerico("<n>")));
}

void collect(current: (Param) `<Identifier n> : <Type t>`, Collector c) = declararParam(n, t, c);
void collect(current: (Param) `val <Identifier n> : <Type t>`, Collector c) = declararParam(n, t, c);
void collect(current: (Param) `var <Identifier n> : <Type t>`, Collector c) = declararParam(n, t, c);

void declararParam(Identifier n, Type t, Collector c) {
  c.define("<n>", rolParam(), n, defType(t));
  collect(t, c);
}

void collect(current: (Attribute) `val <Identifier n> : <Type t> = <Expression e>`, Collector c) = declarar(n, rolVal(), t, e, c);
void collect(current: (Attribute) `var <Identifier n> : <Type t> = <Expression e>`, Collector c) = declarar(n, rolVar(), t, e, c);
void collect(current: (Attribute) `val <Identifier n> = <Expression e>`, Collector c) = declarar(n, rolVal(), e, c);
void collect(current: (Attribute) `var <Identifier n> = <Expression e>`, Collector c) = declarar(n, rolVar(), e, c);

// Metodo dentro de un objeto: nombre(params): Tipo = cuerpo
void collect(MethodDecl current, Collector c) {
  if (appl(_, list[Tree] hijos) := current, Tree h <- hijos, Identifier n := h) {
    c.define("<n>", rolMetodo(), n, defType(tipoDesconocido()));
  }
  c.enterScope(current);
    collectHijos(current, c);
  c.leaveScope(current);
}

// Clase.metodo = method(...) ...
void collect(MethodAssignment current, Collector c) {
  c.enterScope(current);
    collectHijos(current, c);
  c.leaveScope(current);
}

// Regla: la clase de Clase.metodo debe existir
void collect(current: (MethodPath) `<ClassName cl> . <{Identifier "."}+ ids>`, Collector c) {
  c.use(cl, {rolClase()});
}

// ---------- Tipos escritos por el programador ----------

void collect(current: (Type) `<ClassName n>`, Collector c) {
  str nombre = "<n>";
  if (nombre in basicos) {
    c.fact(current, basicos[nombre]);
  } else if (nombre == "List") {
    c.fact(current, tipoLista(tipoDesconocido()));
  } else {
    c.use(n, {rolClase(), rolGenerico()});   // Regla: el tipo debe existir
    c.fact(current, n);
  }
}

void collect(current: (Type) `<ClassName n> [ <{Type ","}+ args> ]`, Collector c) {
  if ("<n>" == "List") {
    if (Type el <- args) {
      c.calculate("tipo lista", current, [el], AType (Solver s) { return tipoLista(s.getType(el)); });
    }
  } else {
    c.use(n, {rolClase()});
    c.fact(current, n);
  }
  collect(args, c);
}

void collect(current: (Type) `<ClassName n> <TypeList? tl> -\> <Type r>`, Collector c) {
  if ("<n>" notin basicos && "<n>" != "List") c.use(n, {rolClase(), rolGenerico()});
  c.fact(current, tipoDesconocido());
  collect(tl, r, c);
}

// ---------- Declaraciones ----------

// Con anotacion de tipo: el valor debe ser compatible con el tipo
void declarar(Identifier n, IdRole rol, Type t, Expression e, Collector c) {
  c.define("<n>", rol, n, defType(t));
  c.requireSubType(e, t, error(e, "Se esperaba un valor de tipo %t, pero la expresion es de tipo %t", t, e));
  collect(t, e, c);
}

// Sin anotacion: el tipo se infiere de la expresion
void declarar(Identifier n, IdRole rol, Expression e, Collector c) {
  c.define("<n>", rol, n, defType(e));
  collect(e, c);
}

void collect(current: (ValueDecl) `val <Identifier n> : <Type t> = <Expression e>`, Collector c) = declarar(n, rolVal(), t, e, c);
void collect(current: (ValueDecl) `val <Identifier n> = <Expression e>`, Collector c) = declarar(n, rolVal(), e, c);
void collect(current: (VarDecl) `var <Identifier n> : <Type t> = <Expression e>`, Collector c) = declarar(n, rolVar(), t, e, c);
void collect(current: (VarDecl) `var <Identifier n> = <Expression e>`, Collector c) = declarar(n, rolVar(), e, c);

// ---------- Sentencias ----------

// Regla: la variable (o el primer nombre de a.b.c) debe existir
void collect(current: (PathName) `<{Identifier "."}+ ids>`, Collector c) {
  if (Identifier primero <- ids) c.use(primero, rolesValor);
}

void collect(current: (Assignment) `<PathName p> = <Expression e>`, Collector c) {
  if ((PathName) `<Identifier n>` := p) {
    c.requireSubType(e, n, error(e, "No se puede asignar un valor de tipo %t a %v, que es de tipo %t", e, "<n>", n));
  }
  collect(p, e, c);
}

void collect(current: (IfExpr) `if <BoolExpr cond> then <IfBranch a>`, Collector c) {
  condicion(cond, c);
  collect(cond, a, c);
}

void collect(current: (IfExpr) `if <BoolExpr cond> then <IfBranch a> else <IfBranch b>`, Collector c) {
  condicion(cond, c);
  collect(cond, a, b, c);
}

void condicion(Tree cond, Collector c) {
  c.requireSubType(cond, tipoBool(), error(cond, "La condicion del if debe ser Boolean, pero es %t", cond));
}

void collect(current: (ForStatement) `for ( <Identifier x> in <Expression col> ) <Statement body>`, Collector c) {
  collect(col, c);
  c.enterScope(current);
    c.define("<x>", rolVal(), x, defType([col], AType (Solver s) {
      if (tipoLista(el) := s.getType(col)) return el;
      return tipoDesconocido();
    }));
    collect(body, c);
  c.leaveScope(current);
}

void collect(current: (ForStatement) `for ( <ForVariables vs> \<- <Expression col> ) <Statement body>`, Collector c) {
  collect(col, c);
  c.enterScope(current);
    for (/Identifier v := vs) c.define("<v>", rolVal(), v, defType(tipoDesconocido()));
    collect(body, c);
  c.leaveScope(current);
}

// ---------- Expresiones: pasos directos entre niveles de precedencia ----------

void collect(current: (Expression) `<BoolExpr e>`, Collector c) = pasar(current, e, c);
void collect(current: (BoolExpr) `<BoolAnd e>`, Collector c) = pasar(current, e, c);
void collect(current: (BoolAnd) `<BoolAtomico e>`, Collector c) = pasar(current, e, c);
void collect(current: (BoolAtomico) `<Comparison e>`, Collector c) = pasar(current, e, c);
void collect(current: (Comparison) `<SumExpr e>`, Collector c) = pasar(current, e, c);
void collect(current: (SumExpr) `<MulExpr e>`, Collector c) = pasar(current, e, c);
void collect(current: (MulExpr) `<PowExpr e>`, Collector c) = pasar(current, e, c);
void collect(current: (PowExpr) `<UnaryExpr e>`, Collector c) = pasar(current, e, c);
void collect(current: (UnaryExpr) `<PostFix e>`, Collector c) = pasar(current, e, c);
void collect(current: (UnaryExpr) `<NewSimple e>`, Collector c) = pasar(current, e, c);
void collect(current: (PostFix) `<Primary e>`, Collector c) = pasar(current, e, c);
void collect(current: (Primary) `<Literal e>`, Collector c) = pasar(current, e, c);
void collect(current: (Primary) `<NewExpr e>`, Collector c) = pasar(current, e, c);
void collect(current: (Primary) `( <Expression e> )`, Collector c) = pasar(current, e, c);

// ---------- Expresiones: literales y variables ----------

void collect(current: (Literal) `<IntLiteral i>`, Collector c) { c.fact(current, tipoInt()); }
void collect(current: (Literal) `<BoolLiteral b>`, Collector c) { c.fact(current, tipoBool()); }
void collect(current: (Literal) `<StringLiteral s>`, Collector c) { c.fact(current, tipoString()); }
void collect(current: (Literal) `<CharLiteral ch>`, Collector c) { c.fact(current, tipoChar()); }

// Regla: toda variable usada debe estar declarada
void collect(current: (Primary) `<Identifier n>`, Collector c) {
  c.use(n, rolesValor);
  c.fact(current, n);
}

// ---------- Expresiones: operadores ----------

void collect(current: (SumExpr) `<SumExpr l> + <MulExpr r>`, Collector c) {
  c.calculate("suma", current, [l, r], AType (Solver s) {
    AType a = s.getType(l);
    AType b = s.getType(r);
    if (tipoDesconocido() := a || tipoDesconocido() := b) return tipoDesconocido();
    if (a == tipoInt() && b == tipoInt()) return tipoInt();
    if (a == tipoString() || b == tipoString()) return tipoString();   // concatenacion
    s.report(error(current, "No se puede sumar %t con %t", l, r));
    return tipoDesconocido();
  });
  collect(l, r, c);
}

void aritmetica(str op, Tree current, Tree l, Tree r, Collector c) {
  c.calculate(op, current, [l, r], AType (Solver s) {
    s.requireSubType(l, tipoInt(), error(l, "El operador %v espera Int, pero recibio %t", op, l));
    s.requireSubType(r, tipoInt(), error(r, "El operador %v espera Int, pero recibio %t", op, r));
    return tipoInt();
  });
  collect(l, r, c);
}

void collect(current: (SumExpr) `<SumExpr l> - <MulExpr r>`, Collector c) = aritmetica("-", current, l, r, c);
void collect(current: (MulExpr) `<MulExpr l> * <PowExpr r>`, Collector c) = aritmetica("*", current, l, r, c);
void collect(current: (MulExpr) `<MulExpr l> / <PowExpr r>`, Collector c) = aritmetica("/", current, l, r, c);
void collect(current: (MulExpr) `<MulExpr l> % <PowExpr r>`, Collector c) = aritmetica("%", current, l, r, c);
void collect(current: (PowExpr) `<UnaryExpr l> ** <PowExpr r>`, Collector c) = aritmetica("**", current, l, r, c);

void collect(current: (UnaryExpr) `- <UnaryExpr e>`, Collector c) {
  c.calculate("negativo", current, [e], AType (Solver s) {
    s.requireSubType(e, tipoInt(), error(e, "El signo - espera Int, pero recibio %t", e));
    return tipoInt();
  });
  collect(e, c);
}

// <, >, <=, >= : Int con Int o Char con Char
void ordenar(str op, Tree current, Tree l, Tree r, Collector c) {
  c.calculate(op, current, [l, r], AType (Solver s) {
    AType a = s.getType(l);
    AType b = s.getType(r);
    bool ok = a in {tipoInt(), tipoChar(), tipoDesconocido()} && esSubtipo(a, b) && esSubtipo(b, a);
    if (!ok) s.report(error(current, "No se puede comparar %t con %t usando %v", l, r, op));
    return tipoBool();
  });
  collect(l, r, c);
}

void collect(current: (Comparison) `<SumExpr l> \< <SumExpr r>`, Collector c) = ordenar("\<", current, l, r, c);
void collect(current: (Comparison) `<SumExpr l> \> <SumExpr r>`, Collector c) = ordenar("\>", current, l, r, c);
void collect(current: (Comparison) `<SumExpr l> \<= <SumExpr r>`, Collector c) = ordenar("\<=", current, l, r, c);
void collect(current: (Comparison) `<SumExpr l> \>= <SumExpr r>`, Collector c) = ordenar("\>=", current, l, r, c);

// = y <> : ambos lados deben ser del mismo tipo
void igualdad(str op, Tree current, Tree l, Tree r, Collector c) {
  c.calculate(op, current, [l, r], AType (Solver s) {
    s.requireComparable(l, r, error(current, "No se puede comparar %t con %t", l, r));
    return tipoBool();
  });
  collect(l, r, c);
}

void collect(current: (Comparison) `<SumExpr l> = <SumExpr r>`, Collector c) = igualdad("=", current, l, r, c);
void collect(current: (Comparison) `<SumExpr l> \<\> <SumExpr r>`, Collector c) = igualdad("\<\>", current, l, r, c);

// and, or, neg : solo Boolean
void logico(str op, Tree current, Tree l, Tree r, Collector c) {
  c.calculate(op, current, [l, r], AType (Solver s) {
    s.requireSubType(l, tipoBool(), error(l, "El operador %v espera Boolean, pero recibio %t", op, l));
    s.requireSubType(r, tipoBool(), error(r, "El operador %v espera Boolean, pero recibio %t", op, r));
    return tipoBool();
  });
  collect(l, r, c);
}

void collect(current: (BoolExpr) `<BoolExpr l> or <BoolAnd r>`, Collector c) = logico("or", current, l, r, c);
void collect(current: (BoolAnd) `<BoolAnd l> and <BoolAtomico r>`, Collector c) = logico("and", current, l, r, c);

void collect(current: (BoolAtomico) `neg <BoolAtomico e>`, Collector c) {
  c.calculate("neg", current, [e], AType (Solver s) {
    s.requireSubType(e, tipoBool(), error(e, "neg espera Boolean, pero recibio %t", e));
    return tipoBool();
  });
  collect(e, c);
}

// if como expresion: condicion Boolean y ramas del mismo tipo
void collect(current: (Expression) `if <BoolExpr cond> then <Expression a> else <Expression b>`, Collector c) {
  c.calculate("if-expresion", current, [cond, a, b], AType (Solver s) {
    s.requireSubType(cond, tipoBool(), error(cond, "La condicion del if debe ser Boolean, pero es %t", cond));
    s.requireComparable(a, b, error(current, "Las ramas del if tienen tipos distintos: %t y %t", a, b));
    return (tipoDesconocido() := s.getType(a)) ? s.getType(b) : s.getType(a);
  });
  collect(cond, a, b, c);
}

// ---------- Objetos en expresiones ----------

// Regla: la clase usada en new debe existir
void tipoNuevo(Tree current, ClassName n, Collector c) {
  str nombre = "<n>";
  if (nombre == "List") {
    c.fact(current, tipoLista(tipoDesconocido()));
  } else if (nombre in basicos) {
    c.report(error(n, "No se puede usar new con el tipo basico %v", nombre));
    c.fact(current, basicos[nombre]);
  } else {
    c.use(n, {rolClase()});
    c.fact(current, n);
  }
}

void collect(current: (NewExpr) `new <ClassName n> <TypeList? tl> ( <{Expression ","}* args> ) <ObjectBody? b>`, Collector c) {
  tipoNuevo(current, n, c);
  collect(tl, args, c);
  c.enterScope(current);
    collect(b, c);
  c.leaveScope(current);
}

void collect(current: (NewSimple) `new <ClassName n> <TypeList? tl> <ObjectBody? b>`, Collector c) {
  tipoNuevo(current, n, c);
  collect(tl, c);
  c.enterScope(current);
    collect(b, c);
  c.leaveScope(current);
}

// Accesos y llamadas (x.campo, x.metodo(...)): por ahora no se verifica su tipo
void collect(current: (PostFix) `<PostFix p> . <Identifier n>`, Collector c) {
  c.fact(current, tipoDesconocido());
  collect(p, c);
}

void collect(current: (PostFix) `<PostFix p> ( <{Expression ","}* args> )`, Collector c) {
  c.fact(current, tipoDesconocido());
  collect(p, args, c);
}

// ---------- Punto de entrada (como TModelFromTree del tutorial) ----------

TModel objectilangTModel(Tree pt) {
  if (pt has top) pt = pt.top;
  padres = calcularPadres(pt);
  Collector c = newCollector("objectilang", pt, tconfig(isSubType = esSubtipo));
  collect(pt, c);
  return newSolver(pt, c.run()).run();
}
