import { page, query, route, type Spec } from "@wasp.sh/spec";
import { MainPage } from "./src/MainPage" with { type: "ref" };
import { getModuleContent } from "./src/queries" with { type: "ref" };

export default [
  route("ModuleRoute", "/module", page(MainPage)),
  query(getModuleContent),
] satisfies Spec;
