import path from "path";

const SITE_DIR_PATH = path.resolve(import.meta.dirname, "../..");

/**
 * Returns the site root (the `web/` directory) and fails fast when the script is
 * run from the wrong working directory, since every path is resolved relative
 * to it (build/, versioned_docs/, .docusaurus/, ...).
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
