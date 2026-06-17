import waspVersions from "../../versions.json";

const KNOWN_VERSIONS = new Set<string>(waspVersions);

const FOOTER_LINK_PREFIX = "> Part of the [Wasp docs](https://wasp.sh/";
const LATEST_VERSION = waspVersions[0];

/**
 * Builds the footer appended to each standalone `.md` so an agent that fetched a
 * single page can still find the docs index. Every docs page points at its own
 * version's doc map (the latest version's map for unprefixed `/docs/...` pages);
 * blog and resources, which have no version map, point at llms.txt.
 */
export function buildDocsFooter(route: string): string {
  return `${FOOTER_LINK_PREFIX}${resolveLlmsFile(route)})`;
}

function resolveLlmsFile(route: string): string {
  if (route !== "/docs" && !route.startsWith("/docs/")) {
    return "llms.txt";
  }
  const version = detectDocVersion(route) ?? LATEST_VERSION;
  return `llms-${version}.txt`;
}

/**
 * Matches the footer at the end of a generated `.md` so generate-llm-files can
 * strip it before concatenating pages into llms-full.txt (otherwise it would
 * repeat once per page).
 */
export const DOCS_FOOTER_PATTERN =
  /\n*> Part of the \[Wasp docs\]\(https:\/\/wasp\.sh\/llms[^)]*\.txt\)\s*$/;

/**
 * Returns the version a doc belongs to: a doc served under "/docs/<version>/..."
 * belongs to that version; the latest version (served at "/docs/...") and
 * non-doc pages have no version prefix and return null.
 */
function detectDocVersion(route: string): string | null {
  const match = route.match(/^\/docs\/([^/]+)/);
  return match && KNOWN_VERSIONS.has(match[1]) ? match[1] : null;
}
