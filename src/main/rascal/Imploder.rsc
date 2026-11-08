module Imploder

import AST;
import ParseTree;
import String;

// Convierte el arbol de parseo en el AST.
// Los literales de texto se guardan sin las comillas: "hola" -> cadena("hola"), 'a' -> caracter("a").
Programa convertir(Tree arbol) {
  Programa p = implode(#Programa, arbol);
  return visit (p) {
    case cadena(str s) => cadena(substring(s, 1, size(s) - 1))
    case caracter(str s) => caracter(substring(s, 1, size(s) - 1))
  };
}
