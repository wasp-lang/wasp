/**
 * This module maps the TsAppSpec-facing API to the internal representation of the app (AppSpec Decl).
 * All of the mapping functions are exported so that they can be individually tested.
 */

import * as AppSpec from "../appSpec.js";
import * as TsAppSpec from "./publicApi/tsAppSpec.js";

export function mapApp(
  app: TsAppSpec.App,
  entityNames: string[],
): AppSpec.Decl[] {
  const { name, wasp, title, head, parts } = app;

  const entityRefParser = makeRefParser("Entity", entityNames);

  // TODO: When you add all declarations, see if you can generalize better
  // (e.g., maybe named parameters, maybe putting extractParts inside
  // mapToDecls)
  const pages = extractParts("page", parts);
  const pageDecls = mapToDecls(
    pages,
    "Page",
    (page) => deriveExtImportName(page.component),
    mapPage,
  );

  const routes = extractParts("route", parts);
  const mappedRoutes = routes.map(mapRoute);
  // TODO: derive a real route name (collision-aware); add invariant for route
  // name uniqueness.
  const routeDecls = mappedRoutes.map(({ route }) => ({
    declType: "Route" as const,
    declName: `${route.to.name}Route`,
    declValue: route,
  }));
  // TODO: dedup with explicit page decls when a route uses a shorthand page
  // whose derived name matches an existing page; add invariant for duplicate
  // pages with differing configs.
  const routePageDecls = mappedRoutes.map(({ route, page }) => ({
    declType: "Page" as const,
    declName: route.to.name,
    declValue: page,
  }));

  const queries = extractParts("query", parts);
  const queryDecls = mapToDecls(
    queries,
    "Query",
    (query) => deriveExtImportName(query.fn),
    (query) => mapQuery(query, entityRefParser),
  );

  const actions = extractParts("action", parts);
  const actionDecls = mapToDecls(
    actions,
    "Action",
    (action) => deriveExtImportName(action.fn),
    (action) => mapAction(action, entityRefParser),
  );

  const appDecl = {
    declType: "App" as const,
    declName: name,
    declValue: {
      wasp,
      title,
      head,
      // TODO: add these guys
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
    Page: [...pageDecls, ...routePageDecls],
    Route: routeDecls,
    Query: queryDecls,
    Action: actionDecls,
    // TODO: add these guys
    Job: [],
    Api: [],
    ApiNamespace: [],
    Crud: [],
  });
}

export function mapPage(page: TsAppSpec.Page): AppSpec.Page {
  const { component, authRequired } = page;
  return {
    component: mapExtImport(component),
    authRequired,
  };
}

export function mapRoute(route: TsAppSpec.Route): {
  route: AppSpec.Route;
  page: AppSpec.Page;
} {
  const normalizedPage = normalizeRoutePage(route.page);
  const pageName = deriveExtImportName(normalizedPage.component);
  return {
    route: {
      path: route.path,
      to: { name: pageName, declType: "Page" },
      prerender: undefined,
      lazy: undefined,
    },
    page: mapPage(normalizedPage),
  };
}

export function normalizeRoutePage(
  routePage: TsAppSpec.Route["page"],
): TsAppSpec.Page {
  if ("kind" in routePage) return routePage;
  return { kind: "page", component: routePage };
}

export function mapQuery(
  query: TsAppSpec.Query,
  entityRefParser: RefParser<"Entity">,
): AppSpec.Query {
  const { fn, entities, auth } = query;
  return {
    fn: mapExtImport(fn),
    entities: entities?.map(entityRefParser),
    auth,
  };
}

export function mapAction(
  action: TsAppSpec.Action,
  entityRefParser: RefParser<"Entity">,
): AppSpec.Action {
  const { fn, entities, auth } = action;
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

export type RefParser<T extends AppSpec.DeclType> = (
  name: string,
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
    };
  };
}

function extractParts<Kind extends TsAppSpec.Part["kind"]>(
  id: Kind,
  parts: TsAppSpec.Part[],
): GetPartForKind<Kind>[] {
  return parts.filter((p): p is GetPartForKind<Kind> => p.kind === id);
}

type GetPartForKind<Kind extends TsAppSpec.Part["kind"]> = Extract<
  TsAppSpec.Part,
  { kind: Kind }
>;

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

export function deriveExtImportName(extImport: TsAppSpec.ExtImport): string {
  if ("import" in extImport) {
    return extImport.alias ?? extImport.import;
  }
  return extImport.importDefault;
}

/**
 * The point of this function is to enforce exhaustivness over all declaration
 * types, ensuring we don't forget to include anything.
 * Check the original comment for details: https://github.com/wasp-lang/wasp/pull/2393#discussion_r1866620833
 *
 * TODO: The new spec bundles all parts (queries, actions...) together in the parts array, so
 * there's no need to go through them one by one.
 * We'd likely be better of by:
 *   1. Mapping the entire array with a dispatcher that calls the correct
 *   mapper depending on the part's kind
 *   2. Passing this mapped array into the app spec (which expects them all on
 *   the same level anyway).
 * We'll likely lose some mapping type safety in the process though. Explore
 * when we're done with the port from legacy to the new spec.
 */
function makeDeclsArray(decls: {
  [Type in AppSpec.Decl["declType"]]: AppSpec.GetDeclForType<Type>[];
}): AppSpec.Decl[] {
  return Object.values(decls).flatMap((decl) => [...decl]);
}
