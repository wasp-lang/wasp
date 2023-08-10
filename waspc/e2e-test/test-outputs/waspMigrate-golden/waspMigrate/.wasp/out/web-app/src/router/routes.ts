export type Routes = 
| {to: "/", params?: {}}
| never

export const routes = {
  RootRoute: (options?: OptionalRouteOptions) => interpolatePath("/", {}, options.search, options.hash),
}

type OptionalRouteOptions = {
  search?: Search;
  hash?: Hash;
}

type ParamValue = string | number
type Params = Record<string, ParamValue>
type Search = string[][] | Record<string, string> | string | URLSearchParams
type Hash = string

export function interpolatePath(
  path: string,
  params?: Params,
  search?: Search,
  hash?: Hash,
) {
  const interpolatedPath = params
  ? path.split('/').map((part) => {
      if (part.startsWith(':')) {
        const paramName = part.slice(1)
        return params[paramName]
      }
      return part
    }).join('/')
  : path;
  const interpolatedSearch = search ? `?${new URLSearchParams(search).toString()}` : '';
  const interpolatedHash = hash ? `#${hash}` : '';
  return interpolatedPath + interpolatedSearch + interpolatedHash;
}
