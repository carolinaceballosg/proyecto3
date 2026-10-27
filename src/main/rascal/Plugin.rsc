module Plugin

import IO;
import ParseTree;
import util::Reflective;
import util::LanguageServer;
import Relation;
import Syntax;

set[LanguageService] contribs() = {
  parser(start[Module] (str input, loc origin) {
    return parse(#start[Module], input, origin);
  })
};

void main() {
  registerLanguage(
    language(
      getProjectPathConfig(|project://proyect2|),
      "Objectilang",
      "obj",
      "Plugin",
      "contribs"
    )
  );
}