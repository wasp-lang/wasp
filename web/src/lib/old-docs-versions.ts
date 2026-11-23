import { docsReorganizationRedirectPaths } from "../../docs-reorganization-redirects";
import docsVersions from "../../versions.json";

// The latest docs version is served at /docs, older ones at /docs/{version}.
const DOCS_PATH = "/docs";

// Wasp 0.24 retired the `main.wasp` DSL in favor of the Wasp Spec.
const FIRST_WASP_SPEC_DOCS_VERSION = "0.24";

// Docs pages (paths inside a docs version) whose content differs in each
// version. For example, each migration guide covers a different upgrade.
const VERSION_SPECIFIC_DOC_PATHS = ["/migration-guide"];

// versions.json lists the newest version first.
const oldDocsVersions = docsVersions.slice(1);

/**
 * Old versions that teach the retired DSL. Their syntax no longer works, so we
 * keep them out of search results. They stay online for users of those
 * versions.
 */
export const waspDslDocsVersions = oldDocsVersions.filter((version) =>
  isOlderVersion(version, FIRST_WASP_SPEC_DOCS_VERSION),
);

/**
 * Old versions that use the Wasp Spec. They stay in search results, but they
 * mostly repeat the latest docs, so their pages point the canonical URL to the
 * latest page (see `getLatestDocCanonicalPath`).
 */
const oldWaspSpecDocsVersions = oldDocsVersions.filter(
  (version) => !waspDslDocsVersions.includes(version),
);

/**
 * Finds the latest docs page that a page of an old Wasp Spec version should
 * use as its canonical URL. Returns `undefined` when the page keeps its own
 * canonical URL: pages outside those versions, version-specific pages, and
 * pages with no match in the latest docs.
 *
 * Both the page metadata (`src/theme/DocItem/Metadata`) and the sitemap
 * (`docusaurus.config.ts`) use it, so that the sitemap lists only canonical
 * URLs.
 *
 * @param sitePaths Paths that exist on the site. Must include every page of
 * the latest docs.
 */
export function getLatestDocCanonicalPath(
  path: string,
  sitePaths: Set<string>,
): string | undefined {
  const version = oldWaspSpecDocsVersions.find((version) =>
    isInDocsVersion(path, version),
  );
  if (!version) {
    return undefined;
  }

  // e.g. /docs/0.25/auth/overview -> /auth/overview
  const pathInVersion = removeTrailingSlash(
    path.slice(`${DOCS_PATH}/${version}`.length),
  );
  if (VERSION_SPECIFIC_DOC_PATHS.includes(pathInVersion)) {
    return undefined;
  }

  const latestPath = getRedirectedDocPath(DOCS_PATH + pathInVersion);
  // Index pages of a docs category have a trailing slash in Docusaurus data
  // (`/docs/guides/`), but the site serves them without one (`trailingSlash:
  // false` in docusaurus.config.ts), and the URL with a slash redirects. So we
  // look up both forms, and return the one without a slash.
  const existsOnSite =
    sitePaths.has(latestPath) || sitePaths.has(`${latestPath}/`);
  return existsOnSite ? latestPath : undefined;
}

// A numeric compare orders "0.9" and "0.11.8" before "0.24", as versions.
function isOlderVersion(version: string, otherVersion: string): boolean {
  return version.localeCompare(otherVersion, undefined, { numeric: true }) < 0;
}

function removeTrailingSlash(path: string): string {
  return path.endsWith("/") ? path.slice(0, -1) : path;
}

function isInDocsVersion(path: string, version: string): boolean {
  const versionPath = `${DOCS_PATH}/${version}`;
  return path === versionPath || path.startsWith(`${versionPath}/`);
}

// Follows a redirect from the docs reorganization, e.g. /docs/auth/overview ->
// /docs/features/auth/overview. A page that did not move keeps its path.
function getRedirectedDocPath(path: string): string {
  const redirect = docsReorganizationRedirectPaths.find(
    ({ from }) => from === path,
  );
  // A redirect can point to a section of a page (`#link-component`), and the
  // canonical URL is the whole page.
  return redirect ? redirect.to.split("#")[0] : path;
}
