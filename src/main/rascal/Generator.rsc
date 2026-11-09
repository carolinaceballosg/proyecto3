module Generator

import AST;
import Parser;
import Imploder;
import IO;
import String;
import List;

alias Env = map[str, value];

data Ref = ref(int id);
data Instancia = instancia(str clase, map[str, value] campos);

data Met
  = metExpr(list[str] params, Expression cuerpoExpr)
  | metBloque(list[str] params, Block cuerpoBloq)
  | metVacio(list[str] params)
  ;

data DefClase = defClase(
  str nombre,
  str padre,
  bool tieneParams,
  list[str] params,
  list[Attribute] atributos,
  map[str, Met] metodos
);

map[str, DefClase] clases = ();
map[int, Instancia] heap = ();
int contador = 0;
int anonimas = 0;

list[str] nombresParams(list[Param] ps) = [n | param(_, n, _) <- ps];

Met crearMet(list[Param] ps, cuerpoBloque(Block b)) = metBloque(nombresParams(ps), b);
Met crearMet(list[Param] ps, expresion(Expression e)) = metExpr(nombresParams(ps), e);

DefClase crearClase(str n, str padre, list[ObjectParams] ps, list[ObjectBody] bs)
  = defClase(
      n,
      padre,
      ps != [],
      [p | params(lp) <- ps, param(_, p, _) <- lp],
      [a | cuerpo(cs) <- bs, atributo(a) <- cs],
      (m : crearMet(lp, body) | cuerpo(cs) <- bs, metodo(metodo(_, m, lp, _, body)) <- cs)
    );

void registrar(objeto(objeto(str n, _, list[Heritage] hs, list[ObjectParams] ps, list[ObjectBody] bs))) {
  str padre = "";
  for (herencia(p) <- hs) padre = p;
  clases[n] = crearClase(n, padre, ps, bs);
}

void registrar(asignMetodo(asigna(rutaClase(str cl, list[str] ms), list[Param] ps, _, list[Block] bs))) {
  if (cl notin clases) throw "La clase <cl> no existe";
  Met nuevoMet = (bs == []) ? metVacio(nombresParams(ps)) : metBloque(nombresParams(ps), bs[0]);
  DefClase k = clases[cl];
  k.metodos[last(ms)] = nuevoMet;
  clases[cl] = k;
}

void registrar(asignMetodo(asigna(rutaId(list[str] ids), _, _, _))) {
  println("Aviso: <intercalate(".", ids)> = method... se ignora (use NombreDeClase.metodo)");
}

list[str] linaje(str cl) {
  list[str] r = [];
  while (cl != "") {
    if (cl notin clases) throw "La clase <cl> no existe";
    r = cl + r;
    cl = clases[cl].padre;
  }
  return r;
}

bool tieneMetodo(str cl, str m) {
  while (cl != "" && cl in clases) {
    if (m in clases[cl].metodos) return true;
    cl = clases[cl].padre;
  }
  return false;
}

Met buscarMetodo(str cl, str m) {
  str original = cl;
  while (cl != "" && cl in clases) {
    if (m in clases[cl].metodos) return clases[cl].metodos[m];
    cl = clases[cl].padre;
  }
  throw "La clase <original> no tiene el metodo <m>";
}

void asignarCampo(int id, str n, value v) {
  Instancia inst = heap[id];
  inst.campos[n] = v;
  heap[id] = inst;
}

value instanciar(str cl, list[value] args) {
  if (cl == "List" && "List" notin clases) return args;   // List del lenguaje, salvo que el programa defina su propia clase List
  list[str] cadenaClases = linaje(cl);
  list[str] ps = [];
  for (k <- reverse(cadenaClases)) {
    if (clases[k].tieneParams) { ps = clases[k].params; break; }
  }
  if (size(ps) != size(args)) throw "new <cl>: se esperaban <size(ps)> argumentos y llegaron <size(args)>";
  contador += 1;
  int id = contador;
  heap[id] = instancia(cl, (ps[i] : args[i] | i <- index(ps)));
  Ref r = ref(id);
  for (k <- cadenaClases, attr(_, n, _, e) <- clases[k].atributos) {
    asignarCampo(id, n, eval(e, ("$self": r)));
  }
  return r;
}

value crear(str cl, list[value] args, list[ObjectBody] bs) {
  if (bs != [] && (cl != "List" || "List" in clases)) {
    anonimas += 1;
    str nombre = "<cl>$<anonimas>";
    clases[nombre] = crearClase(nombre, cl, [], bs);
    cl = nombre;
  }
  return instanciar(cl, args);
}

value llamar(value obj, str m, list[value] args) {
  if (ref(int id) := obj) {
    if (m == "getClass" && args == [] && !tieneMetodo(heap[id].clase, m)) return heap[id].clase;
    Met met = buscarMetodo(heap[id].clase, m);
    if (size(met.params) != size(args)) throw "<m>: se esperaban <size(met.params)> argumentos y llegaron <size(args)>";
    Env entorno = ("$self": obj) + (met.params[i] : args[i] | i <- index(met.params));
    switch (met) {
      case metExpr(_, Expression e): return eval(e, entorno);
      case metBloque(_, Block b): return resultado(ejecutarBloque(b.statements, entorno));
      case metVacio(_): throw "El metodo <m> no tiene cuerpo";
    }
  }
  if (list[value] xs := obj) return metodoLista(xs, m, args);
  if (str s := obj) return metodoString(s, m, args);
  throw "No se puede llamar <m> sobre <texto(obj)>";
}

value metodoLista(list[value] xs, "size", []) = size(xs);
value metodoLista(list[value] xs, "isEmpty", []) = isEmpty(xs);
value metodoLista(list[value] xs, "get", [int i]) = xs[i];
value metodoLista(list[value] xs, "head", []) = xs[0];
value metodoLista(list[value] xs, "tail", []) = tail(xs);
value metodoLista(list[value] xs, "add", [value x]) = xs + [x];
value metodoLista(list[value] xs, "contains", [value x]) = x in xs;
default value metodoLista(list[value] xs, str m, list[value] args) { throw "Las listas no tienen el metodo <m>"; }

value metodoString(str s, "length", []) = size(s);
value metodoString(str s, "getSimpleName", []) = s;   // x.getClass.getSimpleName
default value metodoString(str s, str m, list[value] args) { throw "Los String no tienen el metodo <m>"; }

value obtener(value v, str campo) {
  if (ref(int id) := v, campo in heap[id].campos) return heap[id].campos[campo];
  return llamar(v, campo, []);
}

value buscar(str n, Env env) {
  if (n in env) return env[n];
  if ("$self" in env, ref(int id) := env["$self"]) {
    if (n in heap[id].campos) return heap[id].campos[n];
    if (tieneMetodo(heap[id].clase, n)) return llamar(env["$self"], n, []);
  }
  throw "Variable no definida: <n>";
}

value rutaValor(list[str] ids, Env env) {
  value v = buscar(ids[0], env);
  for (c <- tail(ids)) v = obtener(v, c);
  return v;
}

str texto(value v) {
  if (ref(int id) := v) {
    Instancia inst = heap[id];
    if (tieneMetodo(inst.clase, "toString")) return texto(llamar(v, "toString", []));
    return "<inst.clase>(<intercalate(", ", ["<k>=<texto(inst.campos[k])>" | k <- inst.campos])>)";
  }
  if (list[value] xs := v) return "[<intercalate(", ", [texto(x) | x <- xs])>]";
  return "<v>";
}

str interpolar(str s, Env env) {
  str r = "";
  int i = 0;
  while (i < size(s)) {
    str ch = s[i];
    if (ch == "\\" && i + 1 < size(s)) {
      r += escapar(s[i..i+2]);
      i += 2;
    } else if (ch == "$" && i + 1 < size(s) && s[i+1] == "{") {
      int fin = cierre(s, i + 2);
      r += texto(evalTexto(s[i+2..fin], env));
      i = fin + 1;
    } else if (ch == "$") {
      int j = i + 1;
      while (j < size(s) && /[a-zA-Z0-9_]/ := s[j]) j += 1;
      r += texto(buscar(s[i+1..j], env));
      i = j;
    } else {
      r += ch;
      i += 1;
    }
  }
  return r;
}

str escapar("\\n") = "\n";
str escapar("\\t") = "\t";
default str escapar(str s) = (size(s) == 2 && s[0] == "\\") ? s[1] : s;

int cierre(str s, int desde) {
  int nivel = 0;
  for (int k <- [desde..size(s)]) {
    if (s[k] == "{") nivel += 1;
    else if (s[k] == "}") {
      if (nivel == 0) return k;
      nivel -= 1;
    }
  }
  throw "Interpolacion sin cerrar: falta }";
}

value evalTexto(str expr, Env env) {
  Programa p = convertir(parsear("method main { println(<expr>) }"));
  if (modulo(_, main(bloque([imprimir(imprime(_, Expression e))]))) := p) return eval(e, env);
  throw "No se pudo leer la expresion: <expr>";
}

value eval(literal(entero(n)), Env env) = n;
value eval(literal(booleano(b)), Env env) = b;
value eval(literal(cadena(s)), Env env) = interpolar(s, env);      // el AST ya guarda el texto sin comillas
value eval(literal(caracter(s)), Env env) = escapar(s);
value eval(variable(n), Env env) = buscar(n, env);
value eval(paren(e), Env env) = eval(e, env);

value eval(suma(l, r), Env env) {
  a = eval(l, env); b = eval(r, env);
  if (int x := a, int z := b) return x + z;
  return texto(a) + texto(b);
}
value eval(resta(l, r), Env env) { int x = aEntero(eval(l, env)); int z = aEntero(eval(r, env)); return x - z; }
value eval(mul(l, r), Env env)   { int x = aEntero(eval(l, env)); int z = aEntero(eval(r, env)); return x * z; }
value eval(div(l, r), Env env)   { int x = aEntero(eval(l, env)); int z = aEntero(eval(r, env)); return x / z; }
value eval(resto(l, r), Env env) { int x = aEntero(eval(l, env)); int z = aEntero(eval(r, env)); return x % z; }
value eval(pot(l, r), Env env) {
  int base = aEntero(eval(l, env)); int exponente = aEntero(eval(r, env));
  int res = 1;
  for (int _ <- [0..exponente]) res *= base;
  return res;
}
value eval(negativo(e), Env env) { int x = aEntero(eval(e, env)); return -x; }

int comparar(value a, value b) {
  if (int x := a, int z := b) return x - z;
  if (str x := a, str z := b) return x < z ? -1 : (x == z ? 0 : 1);
  throw "No se pueden comparar <texto(a)> y <texto(b)>";
}
value eval(menor(l, r), Env env)      = comparar(eval(l, env), eval(r, env)) < 0;
value eval(mayor(l, r), Env env)      = comparar(eval(l, env), eval(r, env)) > 0;
value eval(menorIgual(l, r), Env env) = comparar(eval(l, env), eval(r, env)) <= 0;
value eval(mayorIgual(l, r), Env env) = comparar(eval(l, env), eval(r, env)) >= 0;
value eval(igual(l, r), Env env)      = eval(l, env) == eval(r, env);
value eval(distinto(l, r), Env env)   = eval(l, env) != eval(r, env);

value eval(oLogico(l, r), Env env) { bool x = aBool(eval(l, env)); bool z = aBool(eval(r, env)); return x || z; }
value eval(y(l, r), Env env)       { bool x = aBool(eval(l, env)); bool z = aBool(eval(r, env)); return x && z; }
value eval(no(e), Env env)         { bool x = aBool(eval(e, env)); return !x; }

value eval(siValor(c, entonces, sino), Env env) =
  aBool(eval(c, env)) ? eval(entonces, env) : eval(sino, env);

value eval(nuevo(nuevo(str cl, _, list[Expression] args, list[ObjectBody] bs)), Env env)
  = crear(cl, [eval(a, env) | a <- args], bs);
value eval(nuevoSimple(str cl, _, list[ObjectBody] bs), Env env) = crear(cl, [], bs);
value eval(acceso(obj, str campo), Env env) = obtener(eval(obj, env), campo);
value eval(llamada(acceso(obj, str m), list[Expression] args), Env env)
  = llamar(eval(obj, env), m, [eval(a, env) | a <- args]);
value eval(llamada(variable(str m), list[Expression] args), Env env)
  = llamarPropio(m, [eval(a, env) | a <- args], env);

default value eval(Expression e, Env env) { throw "Expresion no soportada todavia: <e>"; }

int aEntero(value v) { if (int x := v) return x; throw "Se esperaba un entero, llego: <texto(v)>"; }
bool aBool(value v) { if (bool x := v) return x; throw "Se esperaba un booleano, llego: <texto(v)>"; }

value llamarPropio(str m, list[value] args, Env env) {
  if ("$self" in env) return llamar(env["$self"], m, args);
  throw "No se puede llamar <m>(...) fuera de un objeto";
}

Env conUltimo(Env env, value v) = env + ("$ultimo": v);
value resultado(Env env) = ("$ultimo" in env) ? env["$ultimo"] : "";

Env ejecutar(valDecl(valor(n, _, e)), Env env) = env + (n: eval(e, env));
Env ejecutar(varDecl(variable(n, _, e)), Env env) = env + (n: eval(e, env));

Env ejecutar(asignacion(asigna(ruta(list[str] ids), Expression e)), Env env) {
  value v = eval(e, env);
  if (size(ids) == 1) {
    str n = ids[0];
    if (n in env) return env + (n: v);
    if ("$self" in env, ref(int id) := env["$self"], n in heap[id].campos) {
      asignarCampo(id, n, v);
      return env;
    }
    return env + (n: v);
  }
  if (ref(int id) := rutaValor(prefix(ids), env)) {
    asignarCampo(id, last(ids), v);
    return env;
  }
  throw "No se puede asignar a <intercalate(".", ids)>";
}

Env ejecutar(imprimir(imprime(t, e)), Env env) {
  if (t == "println") println(texto(eval(e, env)));
  else print(texto(eval(e, env)));
  return env;
}

Env ejecutar(condicional(si(c, entonces, sino)), Env env) {
  if (aBool(eval(c, env))) return rama(entonces, env);
  for (s <- sino) return rama(s, env);
  return env;
}

Env rama(ramaExpr(e), Env env) = conUltimo(env, eval(e, env));
Env rama(ramaPrint(p), Env env) = ejecutar(imprimir(p), env);

Env ejecutar(llamadaStmt(stmtLlamada(ruta(list[str] ids), list[Expression] args, list[StmtSuffix] sufs)), Env env) {
  list[value] vs = [eval(a, env) | a <- args];
  value v = (size(ids) == 1) ? llamarPropio(ids[0], vs, env) : llamar(rutaValor(prefix(ids), env), last(ids), vs);
  return conUltimo(env, aplicarSufijos(v, sufs, env));
}

Env ejecutar(nuevoStmt(NewExpr ne, list[StmtSuffix] sufs), Env env)
  = conUltimo(env, aplicarSufijos(eval(nuevo(ne), env), sufs, env));
Env ejecutar(nuevoSimpleStmt(Expression e), Env env) = conUltimo(env, eval(e, env));
Env ejecutar(refStmt(ruta(list[str] ids)), Env env) = conUltimo(env, rutaValor(ids, env));

value aplicarSufijos(value v, list[StmtSuffix] sufs, Env env) {
  int i = 0;
  while (i < size(sufs)) {
    if (sufAcceso(str m) := sufs[i]) {
      if (i + 1 < size(sufs), sufLlamada(list[Expression] args) := sufs[i + 1]) {
        v = llamar(v, m, [eval(a, env) | a <- args]);
        i += 2;
      } else {
        v = obtener(v, m);
        i += 1;
      }
    } else {
      throw "Llamada sin nombre de metodo";
    }
  }
  return v;
}

Env ejecutar(bucle(para(enLista(str x, Expression col), Statement body)), Env env) = recorrer([x], eval(col, env), body, env);
Env ejecutar(bucle(para(flecha(una(str x), Expression col), Statement body)), Env env) = recorrer([x], eval(col, env), body, env);
Env ejecutar(bucle(para(flecha(varias(list[str] xs), Expression col), Statement body)), Env env) = recorrer(xs, eval(col, env), body, env);

Env recorrer(list[str] nombres, value col, Statement body, Env env) {
  if (list[value] elems := col) {
    for (el <- elems) {
      Env dentro = env;
      if (size(nombres) == 1) {
        dentro[nombres[0]] = el;
      } else if (list[value] partes := el, size(partes) == size(nombres)) {
        for (i <- index(nombres)) dentro[nombres[i]] = partes[i];
      } else {
        throw "for: no se puede separar <texto(el)> en <size(nombres)> partes";
      }
      dentro = ejecutar(body, dentro);
      env = (k : dentro[k] | k <- env);
    }
    return env;
  }
  throw "for: <texto(col)> no es una lista";
}

default Env ejecutar(Statement s, Env env) { throw "Sentencia no soportada todavia: <s>"; }

Env ejecutarBloque(list[Statement] ss, Env env) {
  for (s <- ss) env = ejecutar(s, env);
  return env;
}

void correr(loc archivo) {
  Programa p = convertir(parsear(readFile(archivo)));
  clases = ();
  heap = ();
  contador = 0;
  anonimas = 0;
  if (modulo(list[Declaration] ds, main(bloque(ss))) := p) {
    for (d:objeto(_) <- ds) registrar(d);  
    for (d:asignMetodo(_) <- ds) registrar(d);     
    ejecutarBloque(ss, ());
  }
}