import { page, query, route, type Spec } from "@wasp.sh/spec";
import { MainPage } from "./src/MainPage" with { type: "ref" };
import { getModuleContent } from "./src/queries" with { type: "ref" };

type ModuleOptions = {
  prefix: string;
};

export default function getModuleSpec(options: ModuleOptions): Spec {
  return [
    route("ModuleRoute", options.prefix, page(MainPage)),
    query(getModuleContent),
  ];
}
