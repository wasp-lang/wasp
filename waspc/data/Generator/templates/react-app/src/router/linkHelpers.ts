export type ParamValue = string | number
export type Params = { [name: string]: ParamValue }
export type Search = string[][] | Record<string, string> | string | URLSearchParams

export function interpolatePath(
  path: string,
  params?: Params,
  search?: Search,
  hash?: string,
) {
  const interpolatedPath = params ? interpolatePathParams(path, params) : path;
  const interpolatedSearch = search ? `?${new URLSearchParams(search).toString()}` : '';
  const interpolatedHash = hash ? `#${hash}` : '';
  return interpolatedPath + interpolatedSearch + interpolatedHash;
}

function interpolatePathParams(path: string, params: Params) {
  return path.split('/').map((part) => {
    if (part.startsWith(':')) {
      const paramName = part.slice(1)
      return params[paramName]
    }
    return part
  }).join('/')
}
