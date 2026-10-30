import { normalizePrerender } from "../../normalizePrerender.js";
import { getRefObjectDeclarationName } from "../refObject.js";
export const AppSpecDeclTypeForWaspSpecElementKind = {
    page: "Page",
    route: "Route",
    query: "Query",
    action: "Action",
    api: "Api",
    apiNamespace: "ApiNamespace",
    job: "Job",
    crud: "Crud",
};
export function declToRef(decl) {
    return { declType: decl.declType, name: decl.declValue.name };
}
export function mapSpecElement(el, ctx) {
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
            return el;
    }
}
export function mapPageSpec(page, ctx) {
    const { component, authRequired } = page;
    return {
        declType: "Page",
        declValue: {
            name: getRefObjectDeclarationName(page.component),
            component: ctx.parseRefObject(component),
            authRequired,
        },
    };
}
export function mapRouteSpec(route, ctx) {
    const { path, prerender, lazy } = route;
    return {
        declType: "Route",
        declValue: {
            name: route.name,
            path,
            to: ctx.collectSpecElement(route.page),
            prerender: normalizePrerender(prerender, path),
            lazy,
        },
    };
}
export function mapQuerySpec(query, ctx) {
    const { fn, entities, auth } = query;
    return {
        declType: "Query",
        declValue: {
            name: getRefObjectDeclarationName(query.fn),
            fn: ctx.parseRefObject(fn),
            entities: entities?.map(ctx.resolveEntityRef),
            auth,
        },
    };
}
export function mapActionSpec(action, ctx) {
    const { fn, entities, auth } = action;
    return {
        declType: "Action",
        declValue: {
            name: getRefObjectDeclarationName(action.fn),
            fn: ctx.parseRefObject(fn),
            entities: entities?.map(ctx.resolveEntityRef),
            auth,
        },
    };
}
export function mapApiSpec(api, ctx) {
    const { method, path, fn, middlewareConfigFn, entities, auth } = api;
    return {
        declType: "Api",
        declValue: {
            name: getRefObjectDeclarationName(api.fn),
            fn: ctx.parseRefObject(fn),
            middlewareConfigFn: middlewareConfigFn && ctx.parseRefObject(middlewareConfigFn),
            entities: entities?.map(ctx.resolveEntityRef),
            httpRoute: [method, path],
            auth,
        },
    };
}
export function mapApiNamespaceSpec(apiNamespace, ctx) {
    const { middlewareConfigFn, path } = apiNamespace;
    return {
        declType: "ApiNamespace",
        declValue: {
            name: getRefObjectDeclarationName(apiNamespace.middlewareConfigFn),
            middlewareConfigFn: ctx.parseRefObject(middlewareConfigFn),
            path,
        },
    };
}
export function mapJobSpec(job, ctx) {
    const { fn, executor, schedule, entities, performExecutorOptions } = job;
    return {
        declType: "Job",
        declValue: {
            name: getRefObjectDeclarationName(job.fn),
            executor,
            perform: {
                fn: ctx.parseRefObject(fn),
                executorOptions: performExecutorOptions,
            },
            schedule: schedule && mapSchedule(schedule),
            entities: entities?.map(ctx.resolveEntityRef),
        },
    };
}
export function mapSchedule(schedule) {
    const { cron, args, executorOptions } = schedule;
    return {
        cron,
        args,
        executorOptions,
    };
}
export function mapCrudSpec(crud, ctx) {
    const { entity, operations } = crud;
    return {
        declType: "Crud",
        declValue: {
            name: crud.name,
            entity: ctx.resolveEntityRef(entity),
            operations: mapCrudOperations(operations, ctx),
        },
    };
}
export function mapCrudOperations(operations, ctx) {
    const { get, getAll, create, update, delete: del } = operations;
    return {
        get: get && mapCrudOperationOptions(get, ctx),
        getAll: getAll && mapCrudOperationOptions(getAll, ctx),
        create: create && mapCrudOperationOptions(create, ctx),
        update: update && mapCrudOperationOptions(update, ctx),
        delete: del && mapCrudOperationOptions(del, ctx),
    };
}
export function mapCrudOperationOptions(options, ctx) {
    const { isPublic, overrideFn } = options;
    return {
        isPublic,
        overrideFn: overrideFn && ctx.parseRefObject(overrideFn),
    };
}
