/**
 * This module maps the TsAppSpec-facing API to the internal representation of the app (AppSpec Decl).
 * All of the mapping functions are exported so that they can be individually tested.
 */

import * as AppSpec from "../appSpec.js";
import * as TsAppSpec from "./publicApi/tsAppSpec.js";

export function mapTsAppSpecToAppSpecDecls(
  tsAppSpec: TsAppSpec.TsAppSpec,
  entityNames: string[],
): AppSpec.Decl[] {
  const {
    app,
    pages,
    queries,
  } = tsAppSpec;

  const entityRefParser = makeRefParser("Entity", entityNames);

  const pageDecls = mapToDecls(pages, "Page", mapPage);
  const queryDecls = mapToDecls(queries, "Query", (queryConfig) =>
    mapOperation(queryConfig, entityRefParser),
  );

  const appDecl = {
    declType: "App" as const,
    declName: app.name,
    declValue: mapApp(app.config),
  };

  return makeDeclsArray({
    App: [appDecl],
    Page: pageDecls,
    Query: queryDecls,
  });
}

function makeDeclsArray(decls: {
  [Type in AppSpec.Decl["declType"]]?: AppSpec.GetDeclForType<Type>[];
}): AppSpec.Decl[] {
  return Object.values(decls).flatMap((decl) => [...(decl ?? [])]);
}

function mapToDecls<T, DeclType extends AppSpec.Decl["declType"]>(
  configs: Map<string, T>,
  type: DeclType,
  configToDeclValue: (
    config: T,
  ) => AppSpec.GetDeclForType<DeclType>["declValue"],
) {
  return [...configs].map(([name, config]) => ({
    declType: type,
    declName: name,
    declValue: configToDeclValue(config),
  }));
}

export function mapOperation(
  config: TsAppSpec.QueryConfig,
  entityRefParser: RefParser<"Entity">,
): AppSpec.Query {
  const { fn, entities, auth } = config;
  return {
    fn: mapExtImport(fn),
    entities: entities?.map(entityRefParser),
    auth,
  };
}

export function mapExtImport(
  extImport: TsAppSpec.ExtImport,
): AppSpec.ExtImport {
  if ("import" in extImport) {
    return {
      kind: "named",
      name: extImport.import,
      path: extImport.from,
    };
  } else if ("importDefault" in extImport) {
    return {
      kind: "default",
      name: extImport.importDefault,
      path: extImport.from,
    };
  } else {
    throw new Error(
      "Invalid ExtImport: neither `import` nor `importDefault` is defined",
    );
  }
}

export function mapApp(
  app: TsAppSpec.AppConfig,
): AppSpec.App {
  const { title, wasp, head } = app;
  return {
    wasp,
    title,
    head,
    auth: undefined,
    client: undefined,
    server: undefined,
    webSocket: undefined,
    db: undefined,
    emailSender: undefined,
  };
}

export function mapPage(pageConfig: TsAppSpec.PageConfig): AppSpec.Page {
  const { component, authRequired } = pageConfig;
  return {
    component: mapExtImport(component),
    authRequired,
  };
}

export type RefParser<T extends AppSpec.DeclType> = (
  potentialReferences: string,
) => AppSpec.Ref<T>;

export function makeRefParser<T extends AppSpec.DeclType>(
  declType: T,
  declNames: string[],
): RefParser<T> {
  return function parseRef(potentialRef: string): AppSpec.Ref<T> {
    if (!declNames.includes(potentialRef)) {
      throw new Error(`Invalid ${declType} reference: ${potentialRef}`);
    }
    return {
      name: potentialRef,
      declType,
    } as AppSpec.Ref<T>;
  };
}
