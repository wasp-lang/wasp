import type { LoadedVersion } from "@docusaurus/plugin-content-docs";

export interface LlmFilesContext {
  baseUrl: string;
  outDir: string;
  latestWaspVersion: string;
  /** Docs of every Wasp version, latest first, as loaded by the docs plugin. */
  loadedVersions: LoadedVersion[];
  postCollections: PostCollection[];
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
