import path from "path";

const SITE_DIR_NAME = "web";

/**
 * Returns the site root (the `web/` directory) and fails fast when the script is
 * run from the wrong working directory, since every path is resolved relative
 * to it (build/, versioned_docs/, .docusaurus/, ...).
 */
export function getSiteRoot(): string {
  const siteRoot = process.cwd();
  if (path.basename(siteRoot) !== SITE_DIR_NAME) {
    throw new Error(
      `Expected to run from the "${SITE_DIR_NAME}" directory, but the current ` +
        `working directory is "${siteRoot}". Run this script from web/.`,
    );
  }
  return siteRoot;
}
