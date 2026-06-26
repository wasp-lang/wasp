import type {
  LoadedContent,
  LoadedVersion,
} from "@docusaurus/plugin-content-docs";
import type { LoadContext, Plugin } from "@docusaurus/types";

import { generateLlmFiles } from "./generate-llm-files";
import type { VersionedMarkdownDocs } from "./generate-llm-files/context";
import { permalinkMapFromLoadedVersion } from "./generate-llm-files/permalinks";
import { resolveSidebarsForDocsLoadedVersions } from "./generate-llm-files/resolved-sidebars";
import { generateMarkdownFilesForValidHtmlFiles } from "./html-to-md";

const DOCUSAURUS_DOCS_PLUGIN_NAME = "docusaurus-plugin-content-docs";

/**
 * A Docusaurus plugin which generates markdown variant of docs and `llm*.txt` files.
 */
export function markdownDocsDocusaurusPlugin(context: LoadContext): Plugin {
  let docsLoadedContent: LoadedContent | undefined;

  return {
    name: "wasp-markdown-docs",

    allContentLoaded({ allContent }) {
      docsLoadedContent = allContent[DOCUSAURUS_DOCS_PLUGIN_NAME]?.default as
        | LoadedContent
        | undefined;
      if (!docsLoadedContent) {
        throw Error(
          `wasp-markdown-docs: could not find content for "${DOCUSAURUS_DOCS_PLUGIN_NAME}.default".`,
        );
      }
    },

    async postBuild({ outDir, siteConfig }) {
      if (!docsLoadedContent) {
        throw Error(
          "wasp-markdown-docs: allContentLoaded did not run before postBuild.",
        );
      }

      const { loadedVersions } = docsLoadedContent;
      const baseUrl = stripTrailingSlash(siteConfig.url + siteConfig.baseUrl);

      await generateMarkdownFilesForValidHtmlFiles({
        outDir,
        baseUrl,
      });

      await generateLlmFiles({
        baseUrl,
        projectRoot: context.siteDir,
        outDir,
        latestWaspVersion: loadedVersions[0].versionName,
        versionedMarkdownDocs: collectVersionedMarkdownDocs(loadedVersions),
      });
    },
  };
}

function collectVersionedMarkdownDocs(
  loadedVersions: LoadedVersion[],
): VersionedMarkdownDocs[] {
  const versionedMarkdownDocs: VersionedMarkdownDocs[] = [];

  for (const loadedVersion of loadedVersions) {
    const sidebars = resolveSidebarsForDocsLoadedVersions(loadedVersion);
    const permalinkMap = permalinkMapFromLoadedVersion(loadedVersion);

    versionedMarkdownDocs.push({
      waspVersion: loadedVersion.versionName,
      permalinkMap,
      sidebars,
    });
  }

  return versionedMarkdownDocs;
}

function stripTrailingSlash(value: string): string {
  return value.replace(/\/+$/, "");
}
