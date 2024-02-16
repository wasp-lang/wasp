export type RouteDefinitionsToRoutes<Routes extends RoutesDefinition> = RouteDefinitionsToRoutesObj<Routes>[keyof RouteDefinitionsToRoutesObj<Routes>];
export type OptionalRouteOptions = {
    search?: Search;
    hash?: string;
};
export type ParamValue = string | number;
export type Params = {
    [name: string]: ParamValue;
};
export type Search = string[][] | Record<string, string> | string | URLSearchParams;
type RouteDefinitionsToRoutesObj<Routes extends RoutesDefinition> = {
    [K in keyof Routes]: {
        to: Routes[K]['to'];
    } & ParamsFromBuildFn<Routes[K]['build']>;
};
type RoutesDefinition = {
    [name: string]: {
        to: string;
        build: BuildFn;
    };
};
type BuildFn = (params: unknown) => string;
type ParamsFromBuildFn<BF extends BuildFn> = Parameters<BF>[0] extends {
    params: infer Params;
} ? {
    params: Params;
} : {
    params?: never;
};
export {};
