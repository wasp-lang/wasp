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
