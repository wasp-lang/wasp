/**
 * Code found in this module is not meant to be used in user's server or client
 * code. It is used by the Wasp tooling e.g. in the Tailwind config to resolve
 * paths to the project root directory.
 */

import { join as joinPaths } from 'path'

// PUBLIC API
/**
 * Wasp runs the client code in the `web-app` directory which is nested in the
 * .wasp/out/web-app directory. This function resolves a project root dir path
 * to be relative to the `web-app` directory i.e. `../../../projectDirPath`.
 */
export function resolveProjectPath(path: string): string {
  const waspProjectDirFromWebAppDir = '../../../'
  return joinPaths(waspProjectDirFromWebAppDir, path)
}
