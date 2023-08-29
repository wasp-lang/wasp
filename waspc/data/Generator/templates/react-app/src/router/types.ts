type RoutesDefinition = {
  [name: string]: {
    to: string;
    build: (params: unknown) => string;
  }
}

type RouteDefinitionsToRoutesObj<Routes extends RoutesDefinition> = {
  [K in keyof Routes]: {
    to: Routes[K]["to"];
  } & (Parameters<Routes[K]["build"]>[0] extends {
    params: infer Params;
  }
    ? { params: Params }
    : { params?: never });
}

export type RouteDefinitionsToRoutes<
  Routes extends RoutesDefinition
> = RouteDefinitionsToRoutesObj<Routes>[keyof RouteDefinitionsToRoutesObj<Routes>];

export type OptionalRouteOptions = {
  search?: Search;
  hash?: string;
};

export type ParamValue = string | number
export type Params = { [name: string]: ParamValue }
export type Search =
  | string[][]
  | Record<string, string>
  | string
  | URLSearchParams