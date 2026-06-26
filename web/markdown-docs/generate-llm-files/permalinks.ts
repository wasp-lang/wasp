import type { LoadedVersion } from "@docusaurus/plugin-content-docs";

export type PermalinkMap = Map<DocumentId, DocumentPath>;

type DocumentId = string;
type DocumentPath = string;

/**
 * Builds the document id to permalink mapping per docs version from the docs
 * plugin's in-memory loaded content (the same data Docusaurus would otherwise
 * emit to `.docusaurus/globalData.json`).
 */
export function permalinkMapFromLoadedVersion(
  loadedVersion: LoadedVersion,
): PermalinkMap {
  const permalinkMap: PermalinkMap = new Map();
  for (const doc of loadedVersion.docs) {
    permalinkMap.set(doc.id, doc.permalink);
  }

  return permalinkMap;
}
