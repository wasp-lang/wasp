{{={= =}=}}
export type Routes = 
  {=# routes =}
| {to: "{= urlPath =}", params{=^ urlParams =}?{=/ urlParams =}: {{=# urlParams =}{= . =}: string | number;{=/ urlParams =}}}
  {=/ routes =}
| never

export const routes = {
  {=# routes =}
  {=# hasUrlParams =}
  {= name =}: (options: { params: {{=# urlParams =}{= . =}: ParamValue;{=/ urlParams =}} } & OptionalRouteOptions) => interpolatePath("{= urlPath =}", options.params, options.search, options.hash),
  {=/ hasUrlParams =}
  {=^ hasUrlParams =}
  {= name =}: (options?: OptionalRouteOptions) => interpolatePath("{= urlPath =}", {}, options.search, options.hash),
  {=/ hasUrlParams =}
  {=/ routes =}
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
