import { generatePath } from "react-router";
import type { ParamValue, Params, Search } from "./types";

export function interpolatePath(
  path: string,
  params?: Params,
  search?: Search,
  hash?: string,
) {
  const interpolatedPath = params
    ? generatePath(path, toReactRouterParams(params))
    : path;
  const interpolatedSearch = search
    ? `?${new URLSearchParams(search).toString()}`
    : "";
  const interpolatedHash = hash ? `#${hash}` : "";
  return interpolatedPath + interpolatedSearch + interpolatedHash;
}

function toReactRouterParams(
  params: Params,
): Record<string, string | undefined> {
  return Object.fromEntries(
    Object.entries(params).map(([name, value]) => [
      name,
      toReactRouterParamValue(name, value),
    ]),
  );
}

function toReactRouterParamValue(
  name: string,
  value: ParamValue | undefined,
): string | undefined {
  if (value === undefined || value === null) {
    return undefined;
  }
  return name === "*" ? encodeSplat(String(value)) : String(value);
}

// `generatePath` encodes named params but not the splat, so a `?` or `#` in
// the splat would end up in the search or hash.
function encodeSplat(splat: string): string {
  return splat.split("/").map(encodeURIComponent).join("/");
}
