import type { PermalinkMap } from "./permalinks";
import type { Sidebars } from "./resolved-sidebars";

/**
 * Everything the LLM file generators need, supplied by the Docusaurus plugin
 * instead of being read off disk.
 */
export interface MarkdownDocsContext {
  baseUrl: string;
  projectRoot: string;
  outDir: string;
  latestWaspVersion: string;
  versionedMarkdownDocs: VersionedMarkdownDocs[];
}

/** The resolved data necessary for generation of markdown docs. */
export interface VersionedMarkdownDocs {
  waspVersion: string;
  permalinkMap: PermalinkMap;
  sidebars: Sidebars;
}
