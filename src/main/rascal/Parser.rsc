module Parser

import ParseTree;
import Syntax;

Tree parsear(str programa) {
  start[Module] arbol = parse(#start[Module], programa);
  return arbol.top;
}