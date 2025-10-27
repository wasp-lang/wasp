import { AppSpec, GetDeclForType } from "./appSpec";

/**
 * Transformed and restructured app data optimized for the UI
 */
export type WaspAppData = AppSpec & {
  app: GetDeclForType<"App">;
  pages: GetDeclForType<"Page">[];
  routes: GetDeclForType<"Route">[];
  entities: GetDeclForType<"Entity">[];
  actions: GetDeclForType<"Action">[];
  queries: GetDeclForType<"Query">[];
  apis: GetDeclForType<"Api">[];
  jobs: GetDeclForType<"Job">[];
};

export function transformWaspAppData(waspAppData: AppSpec): WaspAppData {
  const appDecl = waspAppData.decls.find((d) => d.declType === "App") as GetDeclForType<"App">;
  const pageDecls = waspAppData.decls.filter((d) => d.declType === "Page") as GetDeclForType<"Page">[];
  const routeDecls = waspAppData.decls.filter((d) => d.declType === "Route") as GetDeclForType<"Route">[];
  const entityDecls = waspAppData.decls.filter((d) => d.declType === "Entity") as GetDeclForType<"Entity">[];
  const actionDecls = waspAppData.decls.filter((d) => d.declType === "Action") as GetDeclForType<"Action">[];
  const queryDecls = waspAppData.decls.filter((d) => d.declType === "Query") as GetDeclForType<"Query">[];
  const apiDecls = waspAppData.decls.filter((d) => d.declType === "Api") as GetDeclForType<"Api">[];
  const jobDecls = waspAppData.decls.filter((d) => d.declType === "Job") as GetDeclForType<"Job">[];

  return {
    ...waspAppData,
    app: appDecl,
    pages: pageDecls,
    routes: routeDecls,
    entities: entityDecls,
    actions: actionDecls,
    queries: queryDecls,
    apis: apiDecls,
    jobs: jobDecls,
  };
}
