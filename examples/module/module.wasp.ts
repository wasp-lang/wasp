import { page, query, route, type Spec } from "@wasp.sh/spec";
import { MainPage } from "./src/MainPage" with { type: "ref" };
import { getModuleContent } from "./src/queries" with { type: "ref" };

export const moduleSpec: Spec = [
  route("ModuleRoute", "/fsm", page(MainPage)),
  query(getModuleContent),
];
