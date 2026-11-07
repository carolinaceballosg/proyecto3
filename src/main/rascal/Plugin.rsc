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
      getProjectPathConfig(|project://proyecto3|),
      "Objectilang",
      {"obj"},
      "Plugin",
      "contribs"
    )
  );
}