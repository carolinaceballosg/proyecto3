module Plugin

import IO;
import ParseTree;
import util::Reflective;
import util::LanguageServer;
import Syntax;

set[LanguageService] contribs() = {
  parsing(parser(#start[Module]), usesSpecialCaseHighlighting = false)
};

void main() {
  registerLanguage(
    language(
      getProjectPathConfig(|project://proyect2|),
      "Objectilang",
      {"obj"},
      "Plugin",
      "contribs"
    )
  );
}