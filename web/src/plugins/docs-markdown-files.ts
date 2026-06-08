import type { LoadedContent } from "@docusaurus/plugin-content-docs";
import type { LoadContext, Plugin } from "@docusaurus/types";
import fs from "fs/promises";
import path from "path";

import { cleanMarkdown, computeDocPermalink } from "../llm/docs-markdown";

// Emits a raw Markdown file next to every built docs page, so that appending
// `.md` to any docs URL returns clean source for agents to fetch.
// e.g. /docs/auth/overview  ->  /docs/auth/overview.md

type DocToWrite = {
  permalink: string;
  sourcePath: string;
};

export default function docsMarkdownFilesPlugin(
  context: LoadContext,
): Plugin<unknown> {
  const { siteDir } = context;
  let docsToWrite: DocToWrite[] = [];

  return {
    name: "docs-markdown-files",

    async allContentLoaded({ allContent }) {
      const docsPlugin = allContent["docusaurus-plugin-content-docs"] as
        | Record<string, LoadedContent>
        | undefined;
      if (!docsPlugin) return;

      const docs: DocToWrite[] = [];
      for (const loadedContent of Object.values(docsPlugin)) {
        for (const version of loadedContent.loadedVersions) {
          for (const doc of version.docs) {
            // Index/category docs get a trailing-slash permalink (/docs/guides/),
            // but the canonical page URL has no slash (trailingSlash: false), so
            // we serve the markdown at /docs/guides.md to match the llms.txt maps.
            const routePath = doc.permalink.replace(/\/+$/, "") || "/";
            warnOnPermalinkDrift(version, doc, routePath);
            docs.push({
              permalink: routePath,
              sourcePath: doc.source.replace(/^@site/, siteDir),
            });
          }
        }
      }
      docsToWrite = docs;
    },

    async postBuild({ outDir }) {
      for (const { permalink, sourcePath } of docsToWrite) {
        const raw = await fs.readFile(sourcePath, "utf8");
        const markdown = toPageMarkdown(raw);
        const outPath = path.join(outDir, `${permalink}.md`);
        await fs.mkdir(path.dirname(outPath), { recursive: true });
        await fs.writeFile(outPath, markdown, "utf8");
      }
      console.log(`docs-markdown-files: wrote ${docsToWrite.length} .md files`);
    },
  };
}

function toPageMarkdown(rawSource: string): string {
  const { body, title } = splitFrontMatter(rawSource);
  const cleaned = cleanMarkdown(body);
  return title ? `# ${title}\n\n${cleaned}\n` : `${cleaned}\n`;
}

// Minimal frontmatter split: we only need the title, and want to drop the
// frontmatter block from the output. Avoids pulling a YAML parser into the
// plugin.
function splitFrontMatter(rawSource: string): { body: string; title?: string } {
  const match = rawSource.match(/^---\n([\s\S]*?)\n---\n?/);
  if (!match) return { body: rawSource };

  const body = rawSource.slice(match[0].length);
  const titleMatch = match[1].match(/^title:\s*(.+)$/m);
  const title = titleMatch
    ? titleMatch[1].trim().replace(/^['"]|['"]$/g, "")
    : undefined;
  return { body, title };
}

// Guards the on-site `.md` links in the llms.txt maps: those are computed by
// computeDocPermalink in the generator, so if our model of Docusaurus routing
// ever disagrees with a real permalink, the links would 404. Warn loudly.
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
