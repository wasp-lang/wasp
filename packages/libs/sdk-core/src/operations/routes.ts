import { HttpMethod } from "../http.js";

export type OperationRoute = { method: HttpMethod.Post; path: string };

export function makeOperationRoute(
  relativeOperationRoute: string,
): OperationRoute {
  return { method: HttpMethod.Post, path: `/${relativeOperationRoute}` };
}
