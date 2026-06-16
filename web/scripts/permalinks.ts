import { readFileSync } from "fs";
import path from "path";

import waspVersions from "../versions.json";

const LATEST_VERSION = waspVersions[0];
const WASP_BASE_URL = "https://wasp.sh";

export type PermalinkMaps = Map<string, Map<string, string>>;

// Authoritative doc-id -> permalink mapping per version, read from the docs
// plugin data Docusaurus emits during the build. Using this (instead of
// deriving routes from file paths) keeps us correct for `slug` overrides and
// index pages, and it automatically matches whichever version the build serves
// at the /docs root.
export function loadPermalinkMaps(siteRoot: string): PermalinkMaps {
  const globalDataPath = path.join(siteRoot, ".docusaurus/globalData.json");
  const globalData = JSON.parse(readFileSync(globalDataPath, "utf8"));
  const docsPlugin = globalData["docusaurus-plugin-content-docs"]?.default;
  if (!docsPlugin?.versions) {
    throw new Error(
      `Could not read docs versions from ${globalDataPath}. Run the build first.`,
    );
  }

  const maps: PermalinkMaps = new Map();
  for (const version of docsPlugin.versions) {
    const docIdToPermalink = new Map<string, string>();
    for (const doc of version.docs) {
      docIdToPermalink.set(doc.id, doc.path);
    }
    maps.set(version.name, docIdToPermalink);
  }
  return maps;
}

// e.g., 'tutorial/01-create.mdx' → 'tutorial/create'
// e.g., 'data-model/entities/index.md' → 'data-model/entities'
export function normalizePathToDocId(filePath: string): string {
  const normalizedPath = filePath
    .replace(/\.(mdx|md)$/, "")
    .replace(/\/index$/, "");

  const pathSegments = normalizedPath.split("/");
  const filename = pathSegments.pop() ?? ""; // This will be the filename or last directory name

  // Remove leading "NN-" or "NN." from the filename part, e.g. "01-create" => "create"
  const cleanFilename = filename.replace(/^\d+[-.]/, "");

  return pathSegments.length > 0
    ? [...pathSegments, cleanFilename].join("/")
    : cleanFilename;
}

// The docs used to link to each other through raw GitHub source URLs (the old
// way of pointing at "markdown docs"). Now that we serve `.md` pages, rewrite
// those links to the served page. Only links into the docs source are touched;
// raw links to source code, assets, or other repos are left as they are.
const RAW_GITHUB_DOC_LINK =
  /https:\/\/raw\.githubusercontent\.com\/wasp-lang\/wasp\/[^\s)]+?\/web\/(?:versioned_docs\/version-([0-9.]+)|docs)\/([^\s)]+?)\.mdx?(?=[)\s]|$)/g;

export function rewriteRawGithubDocLinks(
  markdown: string,
  maps: PermalinkMaps,
): string {
  return markdown.replace(
    RAW_GITHUB_DOC_LINK,
    (whole, version: string | undefined, relativePath: string) => {
      const docVersion = version ?? LATEST_VERSION;
      const docId = normalizePathToDocId(relativePath);
      const permalink = maps.get(docVersion)?.get(docId);
      if (!permalink) {
        return whole;
      }
      return `${WASP_BASE_URL}${permalink.replace(/\/+$/, "")}.md`;
    },
  );
}
