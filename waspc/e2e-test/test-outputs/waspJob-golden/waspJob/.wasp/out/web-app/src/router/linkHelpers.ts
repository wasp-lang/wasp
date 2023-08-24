export type ParamValue = string | number
export type Params = { [name: string]: ParamValue }
export type Search =
  | string[][]
  | Record<string, string>
  | string
  | URLSearchParams

export function interpolatePath(
  path: string,
  params?: Params,
  search?: Search,
  hash?: string
) {
  const interpolatedPath = params ? interpolatePathParams(path, params) : path
  const interpolatedSearch = search
    ? `?${new URLSearchParams(search).toString()}`
    : ''
  const interpolatedHash = hash ? `#${hash}` : ''
  return interpolatedPath + interpolatedSearch + interpolatedHash
}

function interpolatePathParams(path: string, params: Params) {
  const startsWithSlash = path.startsWith("/");
  const interpolatedPath = path
    .split("/")
    .map((part) => {
      if (part.startsWith(":")) {
        const paramName = getParamName(part);
        return params[paramName];
      }
      return part;
    })
    // Drops empty strings
    .filter(Boolean)
    .join("/");
  // Keeps leading slash if it was there
  return startsWithSlash ? `/${interpolatedPath}` : interpolatedPath;
}

// Gets param name from a string like:
// :name -> name
// :name? -> name
function getParamName(paramString: string) {
  if (paramString.endsWith("?")) {
    return paramString.slice(1, -1);
  }
  return paramString.slice(1);
}
