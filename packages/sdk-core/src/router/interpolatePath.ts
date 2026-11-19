import type { ParamValue, Params, Search } from "./types";

export function interpolatePath(
  path: string,
  params?: Params,
  search?: Search,
  hash?: string,
) {
  const interpolatedPath = params ? interpolatePathParams(path, params) : path;
  const interpolatedSearch = search
    ? `?${new URLSearchParams(search).toString()}`
    : "";
  const interpolatedHash = hash ? `#${hash}` : "";
  return interpolatedPath + interpolatedSearch + interpolatedHash;
}

function interpolatePathParams(path: string, params: Params) {
  function mapPathPart(part: string): string {
    if (part === "*") {
      return encodeSplatParam(params["*"]);
    }
    if (part.startsWith(":")) {
      const paramName = extractParamNameFromPathPart(part);
      return encodePathParam(params[paramName]);
    }
    return part;
  }

  const interpolatedPath = path
    .split("/")
    .map(mapPathPart)
    .filter((part) => part !== "")
    .join("/");

  return path.startsWith("/") ? `/${interpolatedPath}` : interpolatedPath;
}

// A missing param encodes to "", so its segment is dropped.
function encodePathParam(value: ParamValue | undefined): string {
  return encodeURIComponent(value ?? "");
}

// A splat can span several segments, so its slashes stay unencoded.
function encodeSplatParam(value: ParamValue | undefined): string {
  return String(value ?? "")
    .split("/")
    .map(encodeURIComponent)
    .join("/");
}

function extractParamNameFromPathPart(paramString: string) {
  if (paramString.endsWith("?")) {
    return paramString.slice(1, -1);
  }
  return paramString.slice(1);
}
