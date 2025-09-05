import type { Params, Search } from "./types";

// PRIVATE API
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
  function mapPathPart(part: string) {
    if (part === '*') {
      return params['*'];
    }
    if (part.startsWith(":")) {
      const paramName = extractParamNameFromPathPart(part);
      return params[paramName];
    }
    return part;
  }

  const interpolatedPath = path
    .split("/")
    .map(mapPathPart)
    .filter(isValidPathPart)
    .join("/");
    
  return path.startsWith("/") ? `/${interpolatedPath}` : interpolatedPath;
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
