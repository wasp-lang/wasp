import type { PluginModule } from "@docusaurus/types";

import { generateMarkdownFilesForValidHtmlFiles } from "./markdown-docs";

export interface LlmFilesPluginOptions {
  skipElementInMarkdownDocsClass: string;
}

/**
 * A Docusaurus plugin which generates markdown variant of docs for valid HTML files
 * and all `llm*.txt` file variants.
 *
 * TODO: migrate `llm*.txt` files generation to this plugin in next PR.
 */
export function docusaurusPluginLlmFiles({
  skipElementInMarkdownDocsClass,
}: LlmFilesPluginOptions): PluginModule {
  return () => ({
    name: "wasp-llm-files",
    async postBuild({ outDir, siteConfig }) {
      const baseUrl = stripTrailingSlash(siteConfig.url + siteConfig.baseUrl);

      await generateMarkdownFilesForValidHtmlFiles({
        outDir,
        baseUrl,
        skipElementInMarkdownDocsClass,
      });
    },
  });
}

function stripTrailingSlash(value: string): string {
  return value.replace(/\/+$/, "");
}
