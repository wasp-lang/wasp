import type { LoadedContent } from "@docusaurus/plugin-content-docs";
import type { LoadContext, Plugin } from "@docusaurus/types";
import fs from "fs/promises";
import path from "path";

import { cleanMarkdown, computeDocPermalink } from "../llm/docs-markdown";

// Emits a raw Markdown file next to every built docs page, so that appending
// `.md` to any docs URL returns clean source for agents to fetch.
// e.g. /docs/auth/overview  ->  /docs/auth/overview.md

interface DocsMarkdownFileToWrite {
  permalink: string;
  docSourcePath: string;
}

export default function docsMarkdownFilesPlugin(
  context: LoadContext,
): Plugin<unknown> {
  const { siteDir } = context;
  let docsMarkdownFilesToWrite: DocsMarkdownFileToWrite[] = [];

  return {
    name: "docs-markdown-files",
    async allContentLoaded({ allContent }) {
      const docsPlugin = allContent["docusaurus-plugin-content-docs"] as
        | { [pluginID: string]: LoadedContent }
        | undefined;
      if (!docsPlugin) {
        throw new Error("Missing `docusaurus-plugin-content-docs` plugin.");
      }

      for (const loadedContent of Object.values(docsPlugin)) {
        for (const loadedVersion of loadedContent.loadedVersions) {
          for (const doc of loadedVersion.docs) {
            // Index/category docs get a trailing-slash permalink (/docs/guides/),
            // but the canonical page URL has no slash (trailingSlash: false), so
            // we serve the markdown at /docs/guides.md to match the llms.txt maps.
            const modifiedPermalink = doc.permalink.replace(/\/+$/, "") || "/";
            warnOnPermalinkDrift(loadedVersion, doc, modifiedPermalink);
            docsMarkdownFilesToWrite.push({
              permalink: modifiedPermalink,
              docSourcePath: doc.source.replace(/^@site/, siteDir),
            });
          }
        }
      }
    },
    async postBuild({ outDir }) {
      for (const { permalink, docSourcePath } of docsMarkdownFilesToWrite) {
        const rawDoc = await fs.readFile(docSourcePath, "utf8");

        const markdownDoc = toMarkdownDoc(rawDoc);
        const markdownDocPath = path.join(outDir, `${permalink}.md`);

        await fs.mkdir(path.dirname(markdownDocPath), { recursive: true });
        await fs.writeFile(markdownDocPath, markdownDoc, "utf8");
      }
      console.log(
        `docs-markdown-files: wrote ${docsMarkdownFilesToWrite.length} .md files`,
      );
    },
  };
}

/**
 * Guards the on-site `.md` links in the llms.txt maps: those are computed by
 * computeDocPermalink in the generator, so if our model of Docusaurus routing
 * ever disagrees with a real permalink, the links would 404. Warn loudly.
 */
function warnOnPermalinkDrift(
  version: { versionName: string; isLast: boolean },
  doc: { id: string; frontMatter: { slug?: unknown } },
  routePath: string,
): void {
  const slug =
    typeof doc.frontMatter.slug === "string" ? doc.frontMatter.slug : undefined;
  const expected = computeDocPermalink(
    version.versionName,
    doc.id,
    slug,
    version.isLast,
  );
  if (expected !== routePath) {
    console.warn(
      `docs-markdown-files: permalink drift for "${doc.id}" ` +
        `(${version.versionName}): computed ${expected}, actual ${routePath}. ` +
        `llms.txt map links may be wrong.`,
    );
  }
}

function toMarkdownDoc(rawDocSource: string): string {
  const { body, title } = splitDocFrontMatter(rawDocSource);
  const cleanedMarkdown = cleanMarkdown(body);
  return title ? `# ${title}\n\n${cleanedMarkdown}\n` : `${cleanedMarkdown}\n`;
}

function splitDocFrontMatter(rawDocSource: string): {
  body: string;
  title?: string;
} {
  const match = rawDocSource.match(/^---\n([\s\S]*?)\n---\n?/);
  if (!match) return { body: rawDocSource };

  const body = rawDocSource.slice(match[0].length);
  const titleMatch = match[1].match(/^title:\s*(.+)$/m);
  const title = titleMatch
    ? titleMatch[1].trim().replace(/^['"]|['"]$/g, "")
    : undefined;
  return { body, title };
}
