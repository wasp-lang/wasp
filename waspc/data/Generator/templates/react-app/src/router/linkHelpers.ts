import type { Params, Search } from "./types";

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
    .map(getPathPartsMapper(params))
    .filter(isValidPathPart)
    .join("/");
  return startsWithSlash ? `/${interpolatedPath}` : interpolatedPath;
}

function getPathPartsMapper(params: Params) {
  return function mapPathPart(part: string): string | number | undefined {
    if (part.startsWith(":")) {
      const paramName = extractParamNameFromPathPart(part);
      return params[paramName];
    }
    return part;
  }
}

function isValidPathPart(part: any): boolean {
  return !!part;
}

function extractParamNameFromPathPart(paramString: string) {
  if (paramString.endsWith("?")) {
    return paramString.slice(1, -1);
  }
  return paramString.slice(1);
}
