import type { LoadedVersion } from "@docusaurus/plugin-content-docs";

import type { MarkdownDocByRoute } from "../markdown-docs";

export interface LlmFilesContext {
  baseUrl: string;
  outDir: string;
  latestWaspVersion: string;
  loadedVersions: LoadedVersion[];
  postCollections: PostCollection[];
  markdownDocByRoute: MarkdownDocByRoute;
}

/**
 * Posts of a single Docusaurus blog plugin (e.g. blog or resources).
 * Used for generation of post collection indexes in `llms.txt` file.
 */
export interface PostCollection {
  sectionTitle: string;
  posts: PostReference[];
}

export interface PostReference {
  title: string;
  url: string;
}
