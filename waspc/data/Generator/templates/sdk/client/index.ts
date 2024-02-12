import { join as joinPaths } from 'path';

// PUBLIC API
/**
 * Wasp runs the client code in the `web-app` directory which is nested in the
 * .wasp/out/web-app directory. This function resolves a project root dir path
 * to be relative to the `web-app` directory i.e. `../../../projectDirPath`.
 */
export function resolveProjectPath(path: string): string {
  return joinPaths('../../../', path);
}

// PUBLIC API
// NOTE: This is enough to cover Operations and our APIs (src/Wasp/AppSpec/Api.hs).
export enum HttpMethod {
	Get = 'GET',
	Post = 'POST',
	Put = 'PUT',
	Delete = 'DELETE',
}

// PUBLIC API
export type Route = { method: HttpMethod; path: string }
