import type {
  ApiDecl,
  AppDecl,
  Decl,
  EntityDecl,
  JobDecl,
  OperationDecl,
  PageDecl,
  Ref,
  RouteDecl,
} from "./spec";
import type { Data } from "./types";

/** Picks the parts of the app's declarations that the graph shows. */
export function getStudioData(decls: Decl[]): Data {
  const app = getDecls<AppDecl>(decls, "App")[0];
  if (!app) throw new Error("The app's spec has no `App` declaration.");

  return {
    app: {
      name: app.name,
      auth: app.auth && {
        userEntity: { name: app.auth.userEntity.name },
        methods: Object.entries(app.auth.methods)
          .filter(([, config]) => config !== null)
          .map(([method]) => method),
      },
    },
    pages: getDecls<PageDecl>(decls, "Page").map((page) => ({
      name: page.name,
      authRequired: page.authRequired,
    })),
    routes: getDecls<RouteDecl>(decls, "Route").map((route) => ({
      name: route.name,
      path: route.path,
      toPage: { name: route.to.name },
    })),
    operations: [
      ...getDecls<OperationDecl>(decls, "Query").map((query) =>
        toOperation(query, "query"),
      ),
      ...getDecls<OperationDecl>(decls, "Action").map((action) =>
        toOperation(action, "action"),
      ),
    ],
    apis: getDecls<ApiDecl>(decls, "Api").map((api) => ({
      name: api.name,
      httpRoute: { method: api.httpRoute[0], path: api.httpRoute[1] },
      auth: api.auth,
      entities: toEntityNames(api.entities),
    })),
    jobs: getDecls<JobDecl>(decls, "Job").map((job) => ({
      name: job.name,
      schedule: job.schedule?.cron ?? null,
      entities: toEntityNames(job.entities),
    })),
    entities: getDecls<EntityDecl>(decls, "Entity").map((entity) => ({
      name: entity.name,
    })),
  };
}

function getDecls<Value>(decls: Decl[], declType: string): Value[] {
  return decls
    .filter((decl) => decl.declType === declType)
    .map((decl) => decl.declValue as Value);
}

function toOperation(operation: OperationDecl, type: "query" | "action") {
  return {
    type,
    name: operation.name,
    auth: operation.auth,
    entities: toEntityNames(operation.entities),
  };
}

function toEntityNames(entities: Ref[] | null) {
  return (entities ?? []).map((entity) => ({ name: entity.name }));
}
