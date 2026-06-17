import { existsSync, readFileSync } from "fs";
import path from "path";

export type DocVersionToPermalinkMap = Map<DocumentVersion, PermalinkMap>;

type DocumentVersion = string;
type PermalinkMap = Map<DocumentId, DocumentPath>;

type DocumentId = string;
type DocumentPath = string;

/**
 * Builds the authoritative doc-id -> permalink mapping per version from the docs
 * plugin data Docusaurus emits during the build.
 */
export function loadPermalinkMaps(siteRoot: string): DocVersionToPermalinkMap {
  const globalDataPath = path.join(siteRoot, ".docusaurus/globalData.json");
  if (!existsSync(globalDataPath)) {
    throw new Error(
      `Could not read docs versions from ${globalDataPath}. Run the build first.`,
    );
  }

  const globalData = JSON.parse(readFileSync(globalDataPath, "utf8"));
  const docsPlugin = globalData["docusaurus-plugin-content-docs"]?.default;
  if (!docsPlugin?.versions) {
    throw new Error(`Could not parse docs versions from ${globalDataPath}.`);
  }

  const docVersionToPermalinkMap: DocVersionToPermalinkMap = new Map();
  for (const version of docsPlugin.versions) {
    const permalinkMap = new Map<DocumentId, DocumentPath>();
    for (const doc of version.docs) {
      permalinkMap.set(doc.id, doc.path);
    }
    docVersionToPermalinkMap.set(version.name, permalinkMap);
  }

  return docVersionToPermalinkMap;
}

/**
 * Turns a doc source path into its Docusaurus doc id.
 *
 * @example 'tutorial/01-create.mdx' → 'tutorial/create'
 * @example 'data-model/entities/index.md' → 'data-model/entities'
 */
export function normalizePathToDocId(filePath: string): string {
  const normalizedPath = filePath
    .replace(/\.(mdx|md)$/, "")
    .replace(/\/index$/, "");

  const pathSegments = normalizedPath.split("/");
  const filename = pathSegments.pop() ?? ""; // This will be the filename or last directory name.

  // Remove leading number ("NN-" or "NN.") from the filename part, e.g. "01-create" => "create".
  const cleanFilename = filename.replace(/^\d+[-.]/, "");

  return pathSegments.length > 0
    ? [...pathSegments, cleanFilename].join("/")
    : cleanFilename;
}
