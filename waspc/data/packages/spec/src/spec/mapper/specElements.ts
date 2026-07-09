import * as AppSpec from "../../appSpec.js";
import { normalizePrerender } from "../../normalizePrerender.js";
import * as WaspSpec from "../publicApi/waspSpec.js";
import { getRefObjectDeclarationName } from "../refObject.js";
import type { AppMapperContext } from "./context.js";

export const AppSpecDeclTypeForWaspSpecElementKind = {
  page: "Page",
  route: "Route",
  query: "Query",
  action: "Action",
  api: "Api",
  apiNamespace: "ApiNamespace",
  job: "Job",
  crud: "Crud",
} as const satisfies Record<
  WaspSpec.SpecElement["kind"],
  AppSpec.Decl["declType"]
>;

export type AppSpecDeclTypeForWaspSpecElement<
  SpecElement extends WaspSpec.SpecElement,
> = (typeof AppSpecDeclTypeForWaspSpecElementKind)[SpecElement["kind"]];

export function declToRef<SpecElement extends WaspSpec.SpecElement>(
  decl: AppSpec.Decl,
): AppSpec.Ref<AppSpecDeclTypeForWaspSpecElement<SpecElement>> {
  // TypeScript can't correlate `decl.declType` with the spec element's kind
  // across `mapWaspSpecElement`'s switch, so we assert the ref type here.
  return { declType: decl.declType, name: decl.declName } as AppSpec.Ref<
    AppSpecDeclTypeForWaspSpecElement<SpecElement>
  >;
}

export function mapSpecElement<T extends WaspSpec.SpecElement>(
  el: T,
  ctx: AppMapperContext,
): AppSpec.Decl {
  switch (el.kind) {
    case "page":
      return mapPageSpec(el, ctx);
    case "route":
      return mapRouteSpec(el, ctx);
    case "query":
      return mapQuerySpec(el, ctx);
    case "action":
      return mapActionSpec(el, ctx);
    case "api":
      return mapApiSpec(el, ctx);
    case "apiNamespace":
      return mapApiNamespaceSpec(el, ctx);
    case "job":
      return mapJobSpec(el, ctx);
    case "crud":
      return mapCrudSpec(el, ctx);
    default:
      return el satisfies never;
  }
}

export function mapPageSpec(
  page: WaspSpec.Page,
  ctx: AppMapperContext,
): AppSpec.GetDeclForType<"Page"> {
  const { component, authRequired } = page;
  return {
    declType: "Page",
    declName: getRefObjectDeclarationName(page.component),
    declValue: {
      component: ctx.emitRefObject(component),
      authRequired,
    },
  };
}

export function mapRouteSpec(
  route: WaspSpec.Route,
  ctx: AppMapperContext,
): AppSpec.GetDeclForType<"Route"> {
  const { path, prerender, lazy } = route;
  return {
    declType: "Route",
    declName: route.name,
    declValue: {
      path,
      to: ctx.emitSpecElementRef(route.page),
      prerender: normalizePrerender(prerender, path),
      lazy,
    },
  };
}

export function mapQuerySpec(
  query: WaspSpec.Query,
  ctx: AppMapperContext,
): AppSpec.GetDeclForType<"Query"> {
  const { fn, entities, auth } = query;
  return {
    declType: "Query",
    declName: getRefObjectDeclarationName(query.fn),
    declValue: {
      fn: ctx.emitRefObject(fn),
      entities: entities?.map(ctx.emitEntityRef),
      auth,
    },
  };
}

export function mapActionSpec(
  action: WaspSpec.Action,
  ctx: AppMapperContext,
): AppSpec.GetDeclForType<"Action"> {
  const { fn, entities, auth } = action;
  return {
    declType: "Action",
    declName: getRefObjectDeclarationName(action.fn),
    declValue: {
      fn: ctx.emitRefObject(fn),
      entities: entities?.map(ctx.emitEntityRef),
      auth,
    },
  };
}

export function mapApiSpec(
  api: WaspSpec.Api,
  ctx: AppMapperContext,
): AppSpec.GetDeclForType<"Api"> {
  const { method, path, fn, middlewareConfigFn, entities, auth } = api;
  return {
    declType: "Api",
    declName: getRefObjectDeclarationName(api.fn),
    declValue: {
      fn: ctx.emitRefObject(fn),
      middlewareConfigFn:
        middlewareConfigFn && ctx.emitRefObject(middlewareConfigFn),
      entities: entities?.map(ctx.emitEntityRef),
      httpRoute: [method, path],
      auth,
    },
  };
}

export function mapApiNamespaceSpec(
  apiNamespace: WaspSpec.ApiNamespace,
  ctx: AppMapperContext,
): AppSpec.GetDeclForType<"ApiNamespace"> {
  const { middlewareConfigFn, path } = apiNamespace;
  return {
    declType: "ApiNamespace",
    declName: getRefObjectDeclarationName(apiNamespace.middlewareConfigFn),
    declValue: {
      middlewareConfigFn: ctx.emitRefObject(middlewareConfigFn),
      path,
    },
  };
}

export function mapJobSpec(
  job: WaspSpec.Job,
  ctx: AppMapperContext,
): AppSpec.GetDeclForType<"Job"> {
  const { fn, executor, schedule, entities, performExecutorOptions } = job;
  return {
    declType: "Job",
    declName: getRefObjectDeclarationName(job.fn),
    declValue: {
      executor,
      perform: {
        fn: ctx.emitRefObject(fn),
        executorOptions: performExecutorOptions,
      },
      schedule: schedule && mapSchedule(schedule),
      entities: entities?.map(ctx.emitEntityRef),
    },
  };
}

export function mapSchedule(schedule: WaspSpec.Schedule): AppSpec.Schedule {
  const { cron, args, executorOptions } = schedule;
  return {
    cron,
    args,
    executorOptions,
  };
}

export function mapCrudSpec(
  crud: WaspSpec.Crud,
  ctx: AppMapperContext,
): AppSpec.GetDeclForType<"Crud"> {
  const { entity, operations } = crud;
  return {
    declType: "Crud",
    declName: crud.name,
    declValue: {
      entity: ctx.emitEntityRef(entity),
      operations: mapCrudOperations(operations, ctx),
    },
  };
}

export function mapCrudOperations(
  operations: WaspSpec.CrudOperations,
  ctx: AppMapperContext,
): AppSpec.CrudOperations {
  const { get, getAll, create, update, delete: del } = operations;
  return {
    get: get && mapCrudOperationOptions(get, ctx),
    getAll: getAll && mapCrudOperationOptions(getAll, ctx),
    create: create && mapCrudOperationOptions(create, ctx),
    update: update && mapCrudOperationOptions(update, ctx),
    delete: del && mapCrudOperationOptions(del, ctx),
  };
}

export function mapCrudOperationOptions(
  options: WaspSpec.CrudOperationOptions,
  ctx: AppMapperContext,
): AppSpec.CrudOperationOptions {
  const { isPublic, overrideFn } = options;
  return {
    isPublic,
    overrideFn: overrideFn && ctx.emitRefObject(overrideFn),
  };
}
