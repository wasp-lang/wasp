import Prism from "prismjs";
import addWaspLangauge from "../prism/wasp";
import addPrismaLanguage from "../prism/prisma";

addPrismaLanguage(Prism);
addWaspLangauge(Prism);
