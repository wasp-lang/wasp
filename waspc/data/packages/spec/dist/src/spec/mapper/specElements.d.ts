import * as AppSpec from "../../appSpec.js";
import * as WaspSpec from "../publicApi/waspSpec.js";
import type { AppMapperContext } from "./context.js";
export declare const AppSpecDeclTypeForWaspSpecElementKind: {
    readonly page: "Page";
    readonly route: "Route";
    readonly query: "Query";
    readonly action: "Action";
    readonly api: "Api";
    readonly apiNamespace: "ApiNamespace";
    readonly job: "Job";
    readonly crud: "Crud";
};
export type AppSpecDeclTypeForWaspSpecElement<SpecElement extends WaspSpec.SpecElement> = (typeof AppSpecDeclTypeForWaspSpecElementKind)[SpecElement["kind"]];
export declare function declToRef<SpecElement extends WaspSpec.SpecElement>(decl: AppSpec.Decl): AppSpec.Ref<AppSpecDeclTypeForWaspSpecElement<SpecElement>>;
export declare function mapSpecElement<T extends WaspSpec.SpecElement>(el: T, ctx: AppMapperContext): AppSpec.Decl;
export declare function mapPageSpec(page: WaspSpec.Page, ctx: AppMapperContext): AppSpec.GetDeclForType<"Page">;
export declare function mapRouteSpec(route: WaspSpec.Route, ctx: AppMapperContext): AppSpec.GetDeclForType<"Route">;
export declare function mapQuerySpec(query: WaspSpec.Query, ctx: AppMapperContext): AppSpec.GetDeclForType<"Query">;
export declare function mapActionSpec(action: WaspSpec.Action, ctx: AppMapperContext): AppSpec.GetDeclForType<"Action">;
export declare function mapApiSpec(api: WaspSpec.Api, ctx: AppMapperContext): AppSpec.GetDeclForType<"Api">;
export declare function mapApiNamespaceSpec(apiNamespace: WaspSpec.ApiNamespace, ctx: AppMapperContext): AppSpec.GetDeclForType<"ApiNamespace">;
export declare function mapJobSpec(job: WaspSpec.Job, ctx: AppMapperContext): AppSpec.GetDeclForType<"Job">;
export declare function mapSchedule(schedule: WaspSpec.Schedule): AppSpec.Schedule;
export declare function mapCrudSpec(crud: WaspSpec.Crud, ctx: AppMapperContext): AppSpec.GetDeclForType<"Crud">;
export declare function mapCrudOperations(operations: WaspSpec.CrudOperations, ctx: AppMapperContext): AppSpec.CrudOperations;
export declare function mapCrudOperationOptions(options: WaspSpec.CrudOperationOptions, ctx: AppMapperContext): AppSpec.CrudOperationOptions;
//# sourceMappingURL=specElements.d.ts.map