import { isDeepStrictEqual } from "node:util";
import * as AppSpec from "../appSpec.js";
import * as TsAppSpec from "./publicApi/tsAppSpec.js";
import { Part } from "./publicApi/tsAppSpec.js";

type GetPartForKind<Id extends Part["part"]> = Extract<Part, { part: Id }>;

function extractParts<Id extends Part["part"]>(
  id: Id,
  parts: Part[],
): GetPartForKind<Id>[] {
  return parts.filter((p): p is GetPartForKind<Id> => p.part === id);
}

export function mapTsAppSpecToAppSpecDecls(
  spec: TsAppSpec.TsAppSpec,
  entityNames: string[],
): AppSpec.Decl[] {
  const entityRefParser = makeRefParser("Entity", entityNames);

  const pages = extractParts("page", spec.parts);
  const pageDecls = buildPageDecls(pages);

  const queries = extractParts("query", spec.parts);
  const queryDecls = buildQueryDecls(queries, entityRefParser);

  const appDecl = {
    declType: "App" as const,
    declName: spec.name,
    declValue: {
      wasp: spec.wasp,
      title: spec.title,
      head: spec.head,
      auth: undefined,
      server: undefined,
      client: undefined,
      db: undefined,
      emailSender: undefined,
      webSocket: undefined,
    },
  };

  return makeDeclsArray({
    App: [appDecl],
    Page: pageDecls,
    Query: queryDecls,
    // TODO: add these guys
    Route: [],
    Action: [],
    Job: [],
    Api: [],
    ApiNamespace: [],
    Crud: [],
  });
}

function buildPageDecls(
  pages: TsAppSpec.Page[],
): AppSpec.GetDeclForType<"Page">[] {
  const pageDecls = mapToDecls(
    pages,
    "Page",
    (page) => deriveExtImportName(page.component),
    mapPage,
  );
  return dedupPageDecls(pageDecls);
}

function buildQueryDecls(
  queries: TsAppSpec.Query[],
  entityRefParser: RefParser<"Entity">,
): AppSpec.GetDeclForType<"Query">[] {
  const queryDecls = mapToDecls(
    queries,
    "Query",
    (query) => deriveExtImportName(query.fn),
    (query) => mapQuery(query, entityRefParser),
  );
  return assertUniqueDeclNames(queryDecls);
}

function mapToDecls<T, DeclType extends AppSpec.Decl["declType"]>(
  items: T[],
  declType: DeclType,
  deriveName: (item: T) => string,
  mapValue: (item: T) => AppSpec.GetDeclForType<DeclType>["declValue"],
) {
  return items.map((item) => ({
    declType,
    declName: deriveName(item),
    declValue: mapValue(item),
  }));
}

function dedupPageDecls(
  decls: AppSpec.GetDeclForType<"Page">[],
): AppSpec.GetDeclForType<"Page">[] {
  const byName = new Map<string, AppSpec.GetDeclForType<"Page">>();
  for (const decl of decls) {
    const existing = byName.get(decl.declName);
    if (existing && !isDeepStrictEqual(existing, decl)) {
      throw new Error(
        `Two page declarations derive the same name "${decl.declName}" but have ` +
          `different configurations. Use an \`alias\` on one of the component ` +
          `ExtImports, or rename a local default-import binding.`,
      );
    }
    byName.set(decl.declName, decl);
  }
  return [...byName.values()];
}

function assertUniqueDeclNames<D extends AppSpec.Decl>(decls: D[]): D[] {
  const seen = new Set<string>();
  for (const decl of decls) {
    if (seen.has(decl.declName)) {
      throw new Error(
        `Duplicate ${decl.declType} name "${decl.declName}". Use \`alias\` on one of the ExtImports.`,
      );
    }
    seen.add(decl.declName);
  }
  return decls;
}

function makeDeclsArray(decls: {
  [Type in AppSpec.Decl["declType"]]: AppSpec.GetDeclForType<Type>[];
}): AppSpec.Decl[] {
  return Object.values(decls).flatMap((decl) => [...decl]);
}

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
