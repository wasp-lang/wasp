import * as AppSpec from "../appSpec.js";
import * as TsAppSpec from "./publicApi/tsAppSpec.js";

export function mapPage(page: TsAppSpec.Page): AppSpec.Page {
  return {
    component: mapExtImport(page.component),
    authRequired: page.authRequired,
  };
}

export function mapExtImport(
  extImport: TsAppSpec.ExtImport,
): AppSpec.ExtImport {
  if ("import" in extImport) {
    return { kind: "named", name: extImport.import, path: extImport.from };
  }
  return {
    kind: "default",
    name: extImport.importDefault,
    path: extImport.from,
  };
}

export function mapQuery(
  query: TsAppSpec.Query,
  entityRefParser: RefParser<"Entity">,
): AppSpec.Query {
  return {
    fn: mapExtImport(query.fn),
    entities: query.entities?.map(entityRefParser),
    auth: query.auth,
  };
}

export function deriveExtImportName(extImport: TsAppSpec.ExtImport): string {
  if ("import" in extImport) {
    return extImport.alias ?? extImport.import;
  }
  return extImport.importDefault;
}

export type RefParser<T extends AppSpec.DeclType> = (
  name: string,
) => AppSpec.Ref<T>;

export function makeRefParser<T extends AppSpec.DeclType>(
  declType: T,
  validNames: string[],
): RefParser<T> {
  return (name: string): AppSpec.Ref<T> => {
    if (!validNames.includes(name)) {
      throw new Error(`Unknown ${declType} reference: "${name}".`);
    }
    return { name, declType };
  };
}
