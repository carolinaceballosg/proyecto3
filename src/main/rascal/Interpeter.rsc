module Interpeter

import AST;
import Parser;
import Imploder;
import IO;
import String;

alias Env = map[str, value];

str texto(value v) = "<v>";

// ---------- Expresiones ----------
value eval(literal(entero(n)), Env env) = n;
value eval(literal(booleano(b)), Env env) = b;
value eval(literal(cadena(s)), Env env) = substring(s, 1, size(s) - 1);
value eval(variable(n), Env env) = env[n];
value eval(paren(e), Env env) = eval(e, env);

value eval(suma(l, r), Env env) {
  a = eval(l, env); b = eval(r, env);
  if (int x := a, int y := b) return x + y;
  return texto(a) + texto(b);
}
value eval(resta(l, r), Env env) { int x = toInt(eval(l, env)); int y = toInt(eval(r, env)); return x - y; }
value eval(mul(l, r), Env env)   { int x = toInt(eval(l, env)); int y = toInt(eval(r, env)); return x * y; }
value eval(div(l, r), Env env)   { int x = toInt(eval(l, env)); int y = toInt(eval(r, env)); return x / y; }
value eval(modulo(l, r), Env env){ int x = toInt(eval(l, env)); int y = toInt(eval(r, env)); return x % y; }
value eval(negativo(e), Env env) { int x = toInt(eval(e, env)); return -x; }

value eval(menor(l, r), Env env)      = toInt(eval(l, env)) < toInt(eval(r, env));
value eval(mayor(l, r), Env env)      = toInt(eval(l, env)) > toInt(eval(r, env));
value eval(menorIgual(l, r), Env env) = toInt(eval(l, env)) <= toInt(eval(r, env));
value eval(mayorIgual(l, r), Env env) = toInt(eval(l, env)) >= toInt(eval(r, env));
value eval(igual(l, r), Env env)      = eval(l, env) == eval(r, env);
value eval(distinto(l, r), Env env)   = eval(l, env) != eval(r, env);

value eval(operation(l, r), Env env) { bool x = toBool(eval(l, env)); bool y = toBool(eval(r, env)); return x || y; }
value eval(y(l, r), Env env)         { bool x = toBool(eval(l, env)); bool z = toBool(eval(r, env)); return x && z; }
value eval(no(e), Env env)           { bool x = toBool(eval(e, env)); return !x; }

default value eval(Expression e, Env env) { throw "Expresion no soportada todavia: <e>"; }

int toInt(value v) { if (int x := v) return x; throw "Se esperaba un entero, llego: <v>"; }
bool toBool(value v) { if (bool x := v) return x; throw "Se esperaba un booleano, llego: <v>"; }

// ---------- Sentencias ----------
Env ejecutar(valDecl(valor(n, _, e)), Env env) = env + (n: eval(e, env));
Env ejecutar(varDecl(variable(n, _, e)), Env env) = env + (n: eval(e, env));

Env ejecutar(asignacion(asigna(ruta([n]), e)), Env env) = env + (n: eval(e, env));

Env ejecutar(imprimir(imprime(t, e)), Env env) {
  if (t == "println") println(texto(eval(e, env)));
  else print(texto(eval(e, env)));
  return env;
}

Env ejecutar(condicional(si(c, entonces, sino)), Env env) {
  if (toBool(eval(c, env))) return rama(entonces, env);
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

// ---------- Punto de entrada ----------
void correr(loc archivo) {
  Programa p = convertir(parsear(readFile(archivo)));
  if (modulo(_, main(bloque(ss))) := p) {
    ejecutarBloque(ss, ());
  }
}