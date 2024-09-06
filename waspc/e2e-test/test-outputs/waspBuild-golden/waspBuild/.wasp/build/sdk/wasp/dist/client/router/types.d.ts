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
        to: GenerateRoute<Routes[K]['to']>;
    } & ParamsFromBuildFn<Routes[K]['build']>;
};
type RoutesDefinition = {
    [name: string]: {
        to: string;
        build: BuildFn;
    };
};
type BuildFn = (params: never) => string;
type ParamsFromBuildFn<BF extends BuildFn> = Parameters<BF>[0] extends {
    params: infer Params;
} ? {
    params: Params;
} : {
    params?: never;
};
type GenerateRoute<S extends string> = S extends '/' ? '/' : `/${JoinPath<JoinSegments<ExplodeOptionalSegments<SplitPath<S>>>>}`;
type ExplodeOptionalSegments<T> = T extends [infer Head, ...infer Tail] ? Head extends '' ? [...ExplodeOptionalSegments<Tail>] : [_ExplodeOptionalSegment<Head>, ...ExplodeOptionalSegments<Tail>] : T;
type _ExplodeOptionalSegment<T> = T extends `:${infer P}` ? {
    segment: T;
} : T extends `${infer S}?` ? {
    optionalSegment: S;
} : {
    segment: T;
};
type Segment = {
    segment: string;
};
type OptionalSegment = {
    optionalSegment: string;
};
type Elem = Segment | OptionalSegment;
type JoinSegments<T extends Elem[]> = T extends [] ? [] : T extends [infer First extends Elem, ...infer Rest extends Elem[]] ? First extends Segment ? [First['segment'], ...JoinSegments<Rest>] : First extends OptionalSegment ? [First['optionalSegment'], ...JoinSegments<Rest>] | JoinSegments<Rest> : [] : [];
type SplitPath<S extends string> = S extends `${infer T}/${infer U}` ? [T, ...SplitPath<U>] : [S];
type JoinPath<T extends string[]> = T extends [infer Only extends string] ? Only : T extends [infer First extends string, ...infer Rest extends string[]] ? `${First}/${JoinPath<Rest>}` : never;
export {};
