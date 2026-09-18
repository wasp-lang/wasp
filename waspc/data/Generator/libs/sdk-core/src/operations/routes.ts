import { HttpMethod } from "../http.js";

// PRIVATE API
export type OperationRoute = { method: HttpMethod.Post; path: string };

// PRIVATE API
export function makeOperationRoute(
  relativeOperationRoute: string,
): OperationRoute {
  return { method: HttpMethod.Post, path: `/${relativeOperationRoute}` };
}
