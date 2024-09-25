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
export type ExpandRouteOnOptionalStaticSegments<S extends string> = S extends '/'
  ? '/'
  : `/${JoinPath<JoinSegments<ExpandOptionalSegments<SplitPath<S>>>>}`

type ExpandOptionalSegments<T> = T extends [infer Head, ...infer Tail]
  ? Head extends ''
    ? [...ExpandOptionalSegments<Tail>]
    : [_ExpandOptionalSegment<Head>, ...ExpandOptionalSegments<Tail>]
  : T

type _ExpandOptionalSegment<T> = T extends `:${infer P}`
  ? // Param segment
    { segment: T }
  : T extends `${infer S}?`
    ? // Optional segment
      { optionalSegment: S }
    : // Regular segment
      { segment: T }

type Segment = { segment: string }
type OptionalSegment = { optionalSegment: string }

type Elem = Segment | OptionalSegment

type JoinSegments<T extends Elem[]> = T extends []
  ? []
  : T extends [infer First extends Elem, ...infer Rest extends Elem[]]
    ? First extends Segment
      ? [First['segment'], ...JoinSegments<Rest>]
      : First extends OptionalSegment
        ? [First['optionalSegment'], ...JoinSegments<Rest>] | JoinSegments<Rest>
        : []
    : []

type SplitPath<S extends string> = S extends `${infer T}/${infer U}`
  ? [T, ...SplitPath<U>]
  : [S]

type JoinPath<T extends string[]> = T extends [infer Only extends string]
  ? Only
  : T extends [infer First extends string, ...infer Rest extends string[]]
    ? `${First}/${JoinPath<Rest>}`
    : never
