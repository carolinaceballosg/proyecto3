module Plugin

import IO;
import ParseTree;
import util::Reflective;
import util::LanguageServer;
import Syntax;
import Checker;


Summary resumen(loc l, start[Module] programa) {
  TModel tm = objectilangTModel(programa);
  return summary(l,
    messages = {<m.at, m> | m <- getMessages(tm), !(m is info)},
    definitions = getUseDef(tm)
  );
}

set[LanguageService] contribs() = {
  parsing(parser(#start[Module]), usesSpecialCaseHighlighting = false),
  analysis(resumen)
};

void main() {
  registerLanguage(
    language(
      getProjectPathConfig(|project://proyecto3|),
      "Objectilang",
      {"ol", "objl", "obj"},
      "Plugin",
      "contribs"
    )
  );
}
