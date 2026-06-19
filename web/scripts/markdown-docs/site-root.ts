import path from "path";
import { fileURLToPath } from "url";

const SCRIPT_DIR = path.dirname(fileURLToPath(import.meta.url));
const SITE_DIR_PATH = path.resolve(SCRIPT_DIR, "../..");

/**
 * Returns the site root and fails fast when the script is
 * run from the wrong working directory.
 */
export function getSiteRoot(): string {
  const cwd = process.cwd();
  if (path.normalize(cwd) !== path.normalize(SITE_DIR_PATH)) {
    throw new Error(
      `Expected to run from "${SITE_DIR_PATH}", but the current working directory is "${cwd}".`,
    );
  }
  return cwd;
}
