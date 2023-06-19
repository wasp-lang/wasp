import Prism from "prismjs";
import 'prismjs/components/prism-json';
import addWaspLangauge from "./prism/wasp";
import addPrismaLanguage from "./prism/prisma";

import "./Main.css";

addPrismaLanguage(Prism);
addWaspLangauge(Prism);

export function RootComponent({ children }) {
  return (
    <div className="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8 py-8">
      {children}
    </div>
  );
}
