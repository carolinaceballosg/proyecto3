module Interprete

import AST;
import Parser;
import Imploder;
import IO;
import String;

alias Env = map[str, value];

str texto(value v) = "<v>";

value eval(literal(entero(n)), Env env) = n;
value eval(literal(booleano(b)), Env env) = b;
value eval(literal(cadena(s)), Env env) = substring(s, 1, size(s) - 1);
value eval(literal(caracter(s)), Env env) = substring(s, 1, size(s) - 1);
value eval(variable(n), Env env) = env[n];
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
value eval(negativo(e), Env env) { int x = aEntero(eval(e, env)); return -x; }

int comparar(value a, value b) {
  if (int x := a, int z := b) return x - z;
  if (str x := a, str z := b) return x < z ? -1 : (x == z ? 0 : 1);
  throw "No se pueden comparar <a> y <b>";
}
value eval(menor(l, r), Env env)      = comparar(eval(l, env), eval(r, env)) < 0;
value eval(mayor(l, r), Env env)      = comparar(eval(l, env), eval(r, env)) > 0;
value eval(menorIgual(l, r), Env env) = comparar(eval(l, env), eval(r, env)) <= 0;
value eval(mayorIgual(l, r), Env env) = comparar(eval(l, env), eval(r, env)) >= 0;
value eval(igual(l, r), Env env)      = eval(l, env) == eval(r, env);
value eval(distinto(l, r), Env env)   = eval(l, env) != eval(r, env);

value eval(oLogico(l, r), Env env) { bool x = aBool(eval(l, env)); bool z = aBool(eval(r, env)); return x || z; }
value eval(y(l, r), Env env)  { bool x = aBool(eval(l, env)); bool z = aBool(eval(r, env)); return x && z; }
value eval(no(e), Env env)    { bool x = aBool(eval(e, env)); return !x; }

value eval(siValor(c, entonces, sino), Env env) =
  aBool(eval(c, env)) ? eval(entonces, env) : eval(sino, env);

default value eval(Expression e, Env env) { throw "Expresion no soportada todavia: <e>"; }

int aEntero(value v) { if (int x := v) return x; throw "Se esperaba un entero, llego: <v>"; }
bool aBool(value v) { if (bool x := v) return x; throw "Se esperaba un booleano, llego: <v>"; }

Env ejecutar(valDecl(valor(n, _, e)), Env env) = env + (n: eval(e, env));
Env ejecutar(varDecl(variable(n, _, e)), Env env) = env + (n: eval(e, env));

Env ejecutar(asignacion(asigna(ruta([n]), e)), Env env) = env + (n: eval(e, env));

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

Env rama(ramaExpr(e), Env env) { eval(e, env); return env; }
Env rama(ramaPrint(p), Env env) = ejecutar(imprimir(p), env);

default Env ejecutar(Statement s, Env env) { throw "Sentencia no soportada todavia: <s>"; }

Env ejecutarBloque(list[Statement] ss, Env env) {
  for (s <- ss) env = ejecutar(s, env);
  return env;
}

void correr(loc archivo) {
  Programa p = convertir(parsear(readFile(archivo)));
  if (modulo(_, main(bloque(ss))) := p) {
    ejecutarBloque(ss, ());
  }
}
