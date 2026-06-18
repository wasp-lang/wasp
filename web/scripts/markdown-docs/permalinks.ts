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
