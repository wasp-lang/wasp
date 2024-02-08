import * as path from 'path';

// PUBLIC API
/**
 * Make a pattern relative from Wasp's web-app directory to the project's root.
 * This enables users to define a glob pattern that is relative to the project's root.
 */
export function makeProjectGlobPattern(globPattern: string): string {
  return path.join('../../../', globPattern);
}
