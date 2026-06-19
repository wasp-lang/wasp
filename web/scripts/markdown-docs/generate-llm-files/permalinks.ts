import { existsSync, readFileSync } from "fs";
import path from "path";

export type WaspVersionToPermalinkMap = Map<string, PermalinkMap>;

export type PermalinkMap = Map<DocumentId, DocumentPath>;

type DocumentId = string;
type DocumentPath = string;

/**
 * Builds the document id to path mapping per Wasp version from the
 * plugin data Docusaurus emits during the build.
 */
export function loadPermalinkMaps(siteRoot: string): WaspVersionToPermalinkMap {
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

  const docVersionToPermalinkMap: WaspVersionToPermalinkMap = new Map();
  for (const version of docsPlugin.versions) {
    const permalinkMap = new Map<DocumentId, DocumentPath>();
    for (const doc of version.docs) {
      permalinkMap.set(doc.id, doc.path);
    }
    docVersionToPermalinkMap.set(version.name, permalinkMap);
  }

  return docVersionToPermalinkMap;
}
