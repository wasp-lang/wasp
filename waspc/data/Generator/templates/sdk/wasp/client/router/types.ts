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
    to: GenerateRoute<Routes[K]['to']>
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

// Optional static segments handling


type SplitPath<S extends string> = S extends `${infer T}/${infer U}`
  ? [T, ...SplitPath<U>]
  : [S]

type DropEmptyStrings<T> = T extends [infer Head, ...infer Tail]
  ? Head extends ''
    ? [...DropEmptyStrings<Tail>]
    : [ExplodeOptionalStatic<Head>, ...DropEmptyStrings<Tail>]
  : T

type ExplodeOptionalStatic<T> = T extends `:${infer P}`
  ? T
  : T extends `${infer S}?`
  ? [S, '']
  : T

type Elem = string | [string, string]

type JoinSegments<T extends Elem[]> = T extends []
  ? []
  : T extends [infer First extends Elem, ...infer Rest extends Elem[]]
  ? First extends string
    ? [First, ...JoinSegments<Rest>]
    : [First[0], ...JoinSegments<Rest>] | JoinSegments<Rest>
  : []

type JoinPath<T extends string[]> = T extends [infer Only extends string]
  ? Only
  : T extends [infer First extends string, ...infer Rest extends string[]]
  ? `${First}/${JoinPath<Rest>}`
  : never

type GenerateRoute<S extends string> = S extends '/'
  ? '/'
  : `/${JoinPath<JoinSegments<ExplodeOptionalStatic<DropEmptyStrings<SplitPath<S>>>>>}`
