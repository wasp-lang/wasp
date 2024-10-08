// PRIVATE API
export type RouteDefinitionsToRoutes<Routes extends RoutesDefinition> =
  RouteDefinitionsToRoutesObj<Routes>[keyof RouteDefinitionsToRoutesObj<Routes>]

// PRIVATE API
export type OptionalRouteOptions = {
  search?: Search
  hash?: string
}

// PRIVATE API
export type ParamValue = string | number
// PRIVATE API
export type Params = { [name: string]: ParamValue }
// PRIVATE API
export type Search =
  | string[][]
  | Record<string, string>
  | string
  | URLSearchParams

type RouteDefinitionsToRoutesObj<Routes extends RoutesDefinition> = {
  [K in keyof Routes]: {
    to: ExpandRouteOnOptionalStaticSegments<Routes[K]['to']>
  } & ParamsFromBuildFn<Routes[K]['build']>
}

type RoutesDefinition = {
  [name: string]: {
    to: string
    build: BuildFn
  }
}

type BuildFn = (params: never) => string

type ParamsFromBuildFn<BF extends BuildFn> = Parameters<BF>[0] extends {
  params: infer Params
}
  ? { params: Params }
  : { params?: never }

// PRIVATE API (sdk)
/**
 * Optional static segments handling: expands routes with optional segments
 * into multiple routes, one for each possible combination of optional segments.
 *
 * For example: /users/tasks?/:id? will be expanded into two routes:
 * - /users/:id?
 * - /users/tasks/:id?
 */
export type ExpandRouteOnOptionalStaticSegments<Route extends string> = JoinPath<
  ExpandOptionalSegments<ParseSegments<SplitPath<NonEmptyString<Route>>>>
>;

type ParseSegments<T> = T extends [infer Head, ...infer Tail]
  ? [_ParseSegment<Head>, ...ParseSegments<Tail>]
  : T;

type _ParseSegment<T> = T extends `:${infer P}`
  ? // Param segment
    { segment: T }
  : T extends `${infer S}?`
    ? // Optional segment
      { optionalSegment: S }
    : // Regular segment
      { segment: T };

type Segment = { segment: string };
type OptionalSegment = { optionalSegment: string };

type Elem = Segment | OptionalSegment;

type ExpandOptionalSegments<T extends Elem[]> = T extends []
  ? []
  : T extends [infer First extends Elem, ...infer Rest extends Elem[]]
    ? First extends Segment
      ? [First["segment"], ...ExpandOptionalSegments<Rest>]
      : First extends OptionalSegment
        ? [First["optionalSegment"], ...ExpandOptionalSegments<Rest>] | ExpandOptionalSegments<Rest>
        : []
    : [];

type SplitPath<S extends string> = S extends "/"
  ? [""]
  : S extends `${infer T}/${infer U}`
    ? [T, ...SplitPath<U>]
    : [S];

type JoinPath<T extends string[]> = T extends [infer Only extends string]
  ? Only extends ""
    ? "/"
    : Only
  : T extends [infer First extends string, ...infer Rest extends string[]]
    ? `${First}/${JoinPath<Rest>}`
    : never;

type NonEmptyString<S extends string> = S extends "" ? never : S;
