import type { BlogContent } from "@docusaurus/plugin-content-blog";
import type { LoadedContent } from "@docusaurus/plugin-content-docs";
import type { Plugin } from "@docusaurus/types";

import type { PostCollection } from "./context";
import { generateLlmFiles } from "./generate-llm-files";
import { stripTrailingSlash } from "./helpers";
import { generateMarkdownFilesForValidHtmlFiles } from "./html-to-md";

const DOCUSAURUS_DOCS_PLUGIN_NAME = "docusaurus-plugin-content-docs";
const DOCUSAURUS_BLOG_PLUGIN_NAME = "docusaurus-plugin-content-blog";

/**
 * Blog-plugin instances whose posts are listed in `llms.txt`.
 * The plugin id matches the `id` set in the Docusaurus config.
 */
const POST_COLLECTIONS: { pluginId: string; sectionTitle: string }[] = [
  { pluginId: "blog", sectionTitle: "Blog posts" },
  { pluginId: "resources", sectionTitle: "Resources posts" },
];

/**
 * A Docusaurus plugin which generates markdown variant of docs and `llm*.txt` files.
 */
export function docusaurusPluginWaspLlmFiles(): Plugin {
  let docsLoadedContent: LoadedContent | undefined;
  let blogContentByPluginId: { [pluginId: string]: BlogContent } | undefined;

  return {
    name: "wasp-llm-files",

    allContentLoaded({ allContent }) {
      docsLoadedContent = allContent[DOCUSAURUS_DOCS_PLUGIN_NAME]?.default as
        | LoadedContent
        | undefined;
      if (!docsLoadedContent) {
        throw Error(
          `wasp-llm-files: could not find content for "${DOCUSAURUS_DOCS_PLUGIN_NAME}.default".`,
        );
      }

      blogContentByPluginId = allContent[DOCUSAURUS_BLOG_PLUGIN_NAME] as
        | { [pluginId: string]: BlogContent }
        | undefined;
    },

    async postBuild({ outDir, siteConfig }) {
      if (!docsLoadedContent || !blogContentByPluginId) {
        throw Error(
          "wasp-llm-files: allContentLoaded did not run before postBuild.",
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
        outDir,
        latestWaspVersion: loadedVersions[0].versionName,
        loadedVersions,
        postCollections: collectPostCollections(baseUrl, blogContentByPluginId),
      });
    },
  };
}

function collectPostCollections(
  baseUrl: string,
  blogContentByPluginId: { [pluginId: string]: BlogContent },
): PostCollection[] {
  return POST_COLLECTIONS.map(({ pluginId, sectionTitle }) => {
    const blogContent = blogContentByPluginId?.[pluginId];
    if (!blogContent) {
      throw Error(
        `wasp-llm-files: could not find content for "${DOCUSAURUS_BLOG_PLUGIN_NAME}.${pluginId}".`,
      );
    }

    const posts = blogContent.blogPosts.map((blogPost) => ({
      title: blogPost.metadata.title,
      url: baseUrl + blogPost.metadata.permalink + ".md",
    }));

    return { sectionTitle, posts };
  });
}
