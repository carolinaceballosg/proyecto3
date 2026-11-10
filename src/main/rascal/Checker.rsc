module Checker

extend analysis::typepal::TypePal;
import Syntax;
import ParseTree;
import IO;
import Message;

data AType
  = tipoInt()
  | tipoBool()
  | tipoString()
  | tipoChar()
  | tipoLista(AType elemento)
  | tipoClase(str nombre)
  | tipoGenerico(str nombre)
  | tipoDesconocido()
  ;

data PathRole = extendsPath();

data IdRole
  = claseId()
  | valId()
  | varId()
  | paramId()
  | metodoId()
  | genericoId()
  ;

str prettyAType(tipoInt()) = "Int";
str prettyAType(tipoBool()) = "Boolean";
str prettyAType(tipoString()) = "String";
str prettyAType(tipoChar()) = "Char";
str prettyAType(tipoLista(AType e)) = "List[<prettyAType(e)>]";
str prettyAType(tipoClase(str n)) = n;
str prettyAType(tipoGenerico(str n)) = n;
str prettyAType(tipoDesconocido()) = "?";

map[str, AType] basicos = (
  "Int": tipoInt(), "Integer": tipoInt(),
  "Boolean": tipoBool(), "Bool": tipoBool(),
  "String": tipoString(),
  "Char": tipoChar()
);

set[IdRole] rolesValor = {valId(), varId(), paramId(), metodoId()};

map[str, str] padres = ();
map[str, str] calcularPadres(Tree pt)
  = ("<n>" : "<p>"
    | /(ObjectCreation) `object <ClassName n> <GenericParams? g> extends <ClassName p> <ObjectParams? ps> <ObjectBody? b>` := pt);

map[str, ClassName] arbolPadre = ();

// Clases que define el programa (nombre -> su declaracion completa)
map[str, Tree] declClases = ();
map[str, Tree] calcularDeclClases(Tree pt)
  = ("<n>" : oc
    | /oc:(ObjectCreation) `object <ClassName n> <GenericParams? g> <Heritage? h> <ObjectParams? ps> <ObjectBody? b>` := pt);

// List es la lista del lenguaje, salvo que el programa defina su propia clase List
bool listaInterna() = "List" notin declClases;

// Tipo anotado (":" Type)? entre los hijos directos de un nodo
list[Type] tipoAnotado(Tree t) {
  if (appl(_, list[Tree] hijos) := t) {
    for (appl(regular(\opt(_)), [appl(_, list[Tree] partes)]) <- hijos, Type ty <- partes) return [ty];
  }
  return [];
}

DefInfo tipoDeRetorno(Tree decl) {
  if ([Type t] := tipoAnotado(decl)) return defType(t);
  return defType(tipoDesconocido());
}

// ---------- Acceso a miembros (x.campo, x.metodo) con useViaType ----------

// Para buscar miembros de un tipo clase, TypePal busca la clase con ese nombre
tuple[list[str] typeNames, set[IdRole] idRoles] nombresDeTipo(tipoClase(str n)) = <[n], {claseId()}>;
default tuple[list[str] typeNames, set[IdRole] idRoles] nombresDeTipo(AType t) = <[], {}>;

// Miembros de los tipos que no son clases (listas, String, genericos...)
AType tipoSinNombre(AType contenedor, Tree selector, loc scope, Solver s) {
  str m = "<selector>";
  if (tipoLista(AType el) := contenedor) {
    if (m == "size") return tipoInt();
    if (m in {"isEmpty", "contains"}) return tipoBool();
    if (m in {"get", "head"}) return el;
    if (m in {"tail", "add"}) return contenedor;
    s.report(error(selector, "Las listas no tienen el metodo %v", m));
    return tipoDesconocido();
  }
  if (contenedor == tipoString()) {
    if (m == "length") return tipoInt();
    if (m == "getSimpleName") return tipoString();
  }
  if (contenedor in {tipoInt(), tipoBool(), tipoChar(), tipoString()}) {
    s.report(error(selector, "El tipo %t no tiene el miembro %v", contenedor, m));
  }
  return tipoDesconocido();
}
map[str, ClassName] calcularArbolPadre(Tree pt)
  = ("<n>" : p
    | /(ObjectCreation) `object <ClassName n> <GenericParams? g> extends <ClassName p> <ObjectParams? ps> <ObjectBody? b>` := pt);

void conectarAncestros(ClassName n, Collector c) {
  c.addPathToDef(n, {claseId()}, extendsPath());
  str ancestro = "<n>";
  int pasos = 0;
  while (ancestro in arbolPadre && pasos < 100) {
    c.addPathToDef(arbolPadre[ancestro], {claseId()}, extendsPath());
    ancestro = "<arbolPadre[ancestro]>";
    pasos += 1;
  }
}

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
  if (overloadedAType(_) := a || overloadedAType(_) := b) return true;   // metodo redefinido con override
  if (tipoDesconocido() := a || tipoDesconocido() := b) return true;
  if (tipoGenerico(_) := a || tipoGenerico(_) := b) return true;
  if (tipoLista(x) := a, tipoLista(y) := b) return esSubtipo(x, y);
  if (tipoClase(x) := a, tipoClase(y) := b) return esSubclase(x, y);
  return false;
}

bool permitirOverride(set[loc] defs, map[loc, Define] defines)
  = all(loc d <- defs, defines[d].idRole == metodoId());

bool omitir(appl(prod(lit(_), _, _), _)) = true;
bool omitir(appl(prod(layouts(_), _, _), _)) = true;
default bool omitir(Tree _) = false;

void collectHijos(Tree t, Collector c) {
  if (appl(_, list[Tree] hijos) := t) {
    for (Tree h <- hijos, !omitir(h)) collect(h, c);
  }
}

void pasar(Tree current, Tree hijo, Collector c) {
  c.fact(current, hijo);
  collect(hijo, c);
}

void collect(start[Module] current, Collector c) {
  if (appl(_, list[Tree] hijos) := current) collect(hijos[1], c);
}

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

void collect(MainMethod current, Collector c) = collectHijos(current, c);
void collect(ObjectParams current, Collector c) = collectHijos(current, c);
void collect(GenericParams current, Collector c) = collectHijos(current, c);
void collect(ObjectBody current, Collector c) = collectHijos(current, c);
void collect(TypeList current, Collector c) = collectHijos(current, c);
void collect(Statement current, Collector c) = collectHijos(current, c);
void collect(PrintStatement current, Collector c) = collectHijos(current, c);
void collect(CallStmt current, Collector c) = collectHijos(current, c);
void collect(StmtSuffix current, Collector c) = collectHijos(current, c);
void collect(IfBranch current, Collector c) = collectHijos(current, c);
void collect(MethodBody current, Collector c) = collectHijos(current, c);
void collect(Declaration current, Collector c) = collectHijos(current, c);

void collect(current: (ObjectCreation) `object <ClassName n> <GenericParams? gs> <Heritage? h> <ObjectParams? ps> <ObjectBody? b>`, Collector c) {
  c.define("<n>", claseId(), current, defType(tipoClase("<n>")));
  c.enterScope(current);
    for (/(Heritage) `extends <ClassName p>` := h) conectarAncestros(p, c);
    collect(gs, ps, b, c);
  c.leaveScope(current);
}

void collect(GenericSingleParam current, Collector c) {
  for (/ClassName n := current) c.define("<n>", genericoId(), n, defType(tipoGenerico("<n>")));
  for (/Identifier n := current) c.define("<n>", genericoId(), n, defType(tipoGenerico("<n>")));
}

void collect(current: (Param) `<Identifier n> : <Type t>`, Collector c) = declararParam(n, t, c);
void collect(current: (Param) `val <Identifier n> : <Type t>`, Collector c) = declararParam(n, t, c);
void collect(current: (Param) `var <Identifier n> : <Type t>`, Collector c) = declararParam(n, t, c);

void declararParam(Identifier n, Type t, Collector c) {
  c.define("<n>", paramId(), n, defType(t));
  collect(t, c);
}

void collect(current: (Attribute) `val <Identifier n> : <Type t> = <Expression e>`, Collector c) = declarar(n, valId(), t, e, c);
void collect(current: (Attribute) `var <Identifier n> : <Type t> = <Expression e>`, Collector c) = declarar(n, varId(), t, e, c);
void collect(current: (Attribute) `val <Identifier n> = <Expression e>`, Collector c) = declarar(n, valId(), e, c);
void collect(current: (Attribute) `var <Identifier n> = <Expression e>`, Collector c) = declarar(n, varId(), e, c);

void collect(MethodDecl current, Collector c) {
  if (appl(_, list[Tree] hijos) := current, Tree h <- hijos, Identifier n := h) {
    c.define("<n>", metodoId(), n, tipoDeRetorno(current));
    // nombre(...): Tipo = expresion  -> la expresion debe ser de ese tipo
    if ([Type t] := tipoAnotado(current), Tree k <- hijos, (MethodBody) `<Expression e>` := k) {
      c.requireSubType(e, t, error(e, "El metodo %v debe devolver %t, pero devuelve %t", "<n>", t, e));
    }
  }
  c.enterScope(current);
    collectHijos(current, c);
  c.leaveScope(current);
}

void collect(MethodAssignment current, Collector c) {
  c.enterScope(current);
    for (/(MethodPath) `<ClassName cl> . <{Identifier "."}+ ids>` := current) {
      conectarAncestros(cl, c);
      list[Identifier] partes = [i | Identifier i <- ids];
      if ("<cl>" in declClases, size(partes) == 1) {
        c.defineInScope(declClases["<cl>"], "<partes[0]>", metodoId(), partes[0], tipoDeRetorno(current));
      }
    }
    collectHijos(current, c);
  c.leaveScope(current);
}

void collect(MethodPath current, Collector c) { }

void collect(current: (Type) `<ClassName n>`, Collector c) {
  str nombre = "<n>";
  if (nombre in basicos) {
    c.fact(current, basicos[nombre]);
  } else if (nombre == "List" && listaInterna()) {
    c.fact(current, tipoLista(tipoDesconocido()));
  } else {
    c.use(n, {claseId(), genericoId()});
    c.fact(current, n);
  }
}

void collect(current: (Type) `<ClassName n> [ <{Type ","}+ args> ]`, Collector c) {
  if ("<n>" == "List" && listaInterna()) {
    if (Type el <- args) {
      c.calculate("tipo lista", current, [el], AType (Solver s) { return tipoLista(s.getType(el)); });
    }
  } else {
    c.use(n, {claseId()});
    c.fact(current, n);
  }
  collect(args, c);
}

void collect(current: (Type) `<ClassName n> <TypeList? tl> -\> <Type r>`, Collector c) {
  if ("<n>" notin basicos && "<n>" != "List") c.use(n, {claseId(), genericoId()});
  c.fact(current, tipoDesconocido());
  collect(tl, r, c);
}

void declarar(Identifier n, IdRole rol, Type t, Expression e, Collector c) {
  c.define("<n>", rol, n, defType(t));
  c.requireSubType(e, t, error(e, "Se esperaba un valor de tipo %t, pero la expresion es de tipo %t", t, e));
  collect(t, e, c);
}

void declarar(Identifier n, IdRole rol, Expression e, Collector c) {
  c.define("<n>", rol, n, defType(e));
  collect(e, c);
}

void collect(current: (ValueDecl) `val <Identifier n> : <Type t> = <Expression e>`, Collector c) = declarar(n, valId(), t, e, c);
void collect(current: (ValueDecl) `val <Identifier n> = <Expression e>`, Collector c) = declarar(n, valId(), e, c);
void collect(current: (VarDecl) `var <Identifier n> : <Type t> = <Expression e>`, Collector c) = declarar(n, varId(), t, e, c);
void collect(current: (VarDecl) `var <Identifier n> = <Expression e>`, Collector c) = declarar(n, varId(), e, c);

// a.b.c : a debe existir, b debe ser miembro del tipo de a, c miembro del tipo de b...
void collect(current: (PathName) `<{Identifier "."}+ ids>`, Collector c) {
  list[Identifier] partes = [i | Identifier i <- ids];
  c.use(partes[0], rolesValor);
  for (int k <- [1..size(partes)]) {
    if ("<partes[k]>" == "getClass") break;
    c.useViaType(partes[k - 1], partes[k], rolesValor);
  }
}

void collect(current: (Assignment) `<PathName p> = <Expression e>`, Collector c) {
  if ((PathName) `<{Identifier "."}+ ids>` := p) {
    list[Identifier] partes = [i | Identifier i <- ids];
    Identifier destino = partes[size(partes) - 1];
    if (!("getClass" in ["<i>" | i <- partes])) {
      c.requireSubType(e, destino, error(e, "No se puede asignar un valor de tipo %t a %v, que es de tipo %t", e, "<p>", destino));
    }
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
    c.define("<x>", valId(), x, defType([col], AType (Solver s) {
      if (tipoLista(el) := s.getType(col)) return el;
      return tipoDesconocido();
    }));
    collect(body, c);
  c.leaveScope(current);
}

void collect(current: (ForStatement) `for ( <ForVariables vs> \<- <Expression col> ) <Statement body>`, Collector c) {
  collect(col, c);
  c.enterScope(current);
    for (/Identifier v := vs) c.define("<v>", valId(), v, defType(tipoDesconocido()));
    collect(body, c);
  c.leaveScope(current);
}

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

void collect(current: (Literal) `<IntLiteral i>`, Collector c) { c.fact(current, tipoInt()); }
void collect(current: (Literal) `<BoolLiteral b>`, Collector c) { c.fact(current, tipoBool()); }
void collect(current: (Literal) `<StringLiteral s>`, Collector c) { c.fact(current, tipoString()); }
void collect(current: (Literal) `<CharLiteral ch>`, Collector c) { c.fact(current, tipoChar()); }

void collect(current: (Primary) `<Identifier n>`, Collector c) {
  c.use(n, rolesValor);
  c.fact(current, n);
}

void collect(current: (SumExpr) `<SumExpr l> + <MulExpr r>`, Collector c) {
  c.calculate("suma", current, [l, r], AType (Solver s) {
    AType a = s.getType(l);
    AType b = s.getType(r);
    if (tipoDesconocido() := a || tipoDesconocido() := b) return tipoDesconocido();
    if (a == tipoInt() && b == tipoInt()) return tipoInt();
    if (a == tipoString() || b == tipoString()) return tipoString();
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

void igualdad(str op, Tree current, Tree l, Tree r, Collector c) {
  c.calculate(op, current, [l, r], AType (Solver s) {
    s.requireComparable(l, r, error(current, "No se puede comparar %t con %t", l, r));
    return tipoBool();
  });
  collect(l, r, c);
}

void collect(current: (Comparison) `<SumExpr l> = <SumExpr r>`, Collector c) = igualdad("=", current, l, r, c);
void collect(current: (Comparison) `<SumExpr l> \<\> <SumExpr r>`, Collector c) = igualdad("\<\>", current, l, r, c);

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

void collect(current: (Expression) `if <BoolExpr cond> then <Expression a> else <Expression b>`, Collector c) {
  c.calculate("if-expresion", current, [cond, a, b], AType (Solver s) {
    s.requireSubType(cond, tipoBool(), error(cond, "La condicion del if debe ser Boolean, pero es %t", cond));
    s.requireComparable(a, b, error(current, "Las ramas del if tienen tipos distintos: %t y %t", a, b));
    return (tipoDesconocido() := s.getType(a)) ? s.getType(b) : s.getType(a);
  });
  collect(cond, a, b, c);
}

void tipoNuevo(Tree current, ClassName n, Collector c) {
  str nombre = "<n>";
  if (nombre == "List" && listaInterna()) {
    c.fact(current, tipoLista(tipoDesconocido()));
  } else if (nombre in basicos) {
    c.report(error(n, "No se puede usar new con el tipo basico %v", nombre));
    c.fact(current, basicos[nombre]);
  } else {
    c.use(n, {claseId()});
    c.fact(current, n);
  }
}

void collect(current: (NewExpr) `new <ClassName n> <TypeList? tl> ( <{Expression ","}* args> ) <ObjectBody? b>`, Collector c) {
  tipoNuevo(current, n, c);
  collect(tl, args, c);
  c.enterScope(current);
    if ("<b>" != "" && "<n>" notin basicos && "<n>" != "List") conectarAncestros(n, c);
    collect(b, c);
  c.leaveScope(current);
}

void collect(current: (NewSimple) `new <ClassName n> <TypeList? tl> <ObjectBody? b>`, Collector c) {
  tipoNuevo(current, n, c);
  collect(tl, c);
  c.enterScope(current);
    if ("<b>" != "" && "<n>" notin basicos && "<n>" != "List") conectarAncestros(n, c);
    collect(b, c);
  c.leaveScope(current);
}

// x.campo : el campo debe existir en la clase de x (o en sus ancestros) y tiene su tipo
void collect(current: (PostFix) `<PostFix p> . <Identifier n>`, Collector c) {
  if ("<n>" == "getClass") {
    c.fact(current, tipoString());
  } else {
    c.useViaType(p, n, rolesValor);
    c.fact(current, n);
  }
  collect(p, c);
}

// x.metodo(args) : tiene el tipo de retorno del metodo
void collect(current: (PostFix) `<PostFix p> ( <{Expression ","}* args> )`, Collector c) {
  c.fact(current, p);
  collect(p, args, c);
}

TModel objectilangTModel(Tree pt) {
  padres = calcularPadres(pt);
  arbolPadre = calcularArbolPadre(pt);
  declClases = calcularDeclClases(pt);
  Collector c = newCollector("objectilang", pt, tconfig(
    isSubType = esSubtipo,
    mayOverload = permitirOverride,
    getTypeNamesAndRole = nombresDeTipo,
    getTypeInNamelessType = tipoSinNombre
  ));
  collect(pt, c);
  return newSolver(pt, c.run()).run();
}

void revisar(loc archivo) {
  list[Message] msgs = getMessages(objectilangTModel(parse(#start[Module], archivo)));
  if (msgs == []) println("Sin errores de tipo");
  for (m <- msgs) println("linea <m.at.begin.line>: <m.msg>");
}