import fm from "front-matter";
import fs from "fs/promises";
import { glob } from "glob";
import path from "path";

import { getSiteRoot } from "../site-root";
import { WASP_BASE_URL } from "./constants";

const SITE_ROOT = getSiteRoot();
const BLOG_DIR = path.join(SITE_ROOT, "blog");
const BUILD_DIR = path.join(SITE_ROOT, "build");

const LLMS_TXT_INTRO = `# Wasp

> Wasp is a full-stack web framework with batteries included for React, Node.js, and Prisma.
> It handles auth, database, routing, deployment, and more out of the box,
> letting you build production-ready apps faster with less boilerplate.`;

const LLMS_TXT_RESOURCES = `## Other Resources
- [Wasp Developer Discord](https://discord.com/invite/rzdnErX)
- [Open SaaS - Free, Open-Source SaaS Boilerplate built on Wasp](https://opensaas.sh)
- [Wasp GitHub](https://github.com/wasp-lang/wasp)
`;

export async function generateLlmsTxtFile(
  waspVersions: string[],
): Promise<void> {
  const blogPostsIndexSection = await buildBlogPostsIndexSection();
  const llmFilesIndexSection = buildLlmFilesIndexSection(waspVersions);
  const llmFullFilesSection = `## Full Concatenated Documentation Files
Use the same URL pattern as the versioned documentation maps: ${WASP_BASE_URL}llms-full-{version}.txt`;

  const llmsTxtContent = [
    LLMS_TXT_INTRO,
    llmFilesIndexSection,
    llmFullFilesSection,
    blogPostsIndexSection,
    LLMS_TXT_RESOURCES,
  ].join("\n\n");
  const llmsTxtAbsPath = path.join(BUILD_DIR, "llms.txt");

  await fs.writeFile(llmsTxtAbsPath, llmsTxtContent, "utf8");
  console.log("Generated: llms.txt");
}

interface BlogPostReference {
  title: string;
  url: string;
}

/**
 * Generates a blog posts index section.
 *
 * @example
 * Rendered output:
 * ```md
 * ## Blogposts
 * - [Wasp Launch Week #12 - TS Spec aka MeTSamorphosis 🐛🦋](https://wasp.sh/blog/2026/06/05/wasp-launch-week-12-ts-spec)
 * - [How To Build A SaaS with AI. Without Getting Stuck.](https://wasp.sh/blog/2026/06/03/build-a-saas-with-ai-without-getting-stuck)
 * ...
 * ```
 */
async function buildBlogPostsIndexSection(): Promise<string> {
  const blogPostsFileNames = await glob("*.{md,mdx}", {
    cwd: BLOG_DIR,
    nodir: true,
    ignore: ["_*.md", "_*.mdx", "CLAUDE.md"], // Ignore markdown partials.
  });

  if (blogPostsFileNames.length === 0) {
    return "";
  }
  // Assumes we use the `YYYY-MM-DD-slug.md(x)` naming convention.
  blogPostsFileNames.sort((a, b) => b.localeCompare(a));

  const blogPostReferences: BlogPostReference[] = [];
  for (const blogPostFileName of blogPostsFileNames) {
    const url = generateBlogPostUrl(blogPostFileName);

    const blogPostAbsFilePath = path.join(BLOG_DIR, blogPostFileName);
    const blogPostFrontMatter = fm<{ [key: string]: string | undefined }>(
      await fs.readFile(blogPostAbsFilePath, "utf8"),
    );
    const title = blogPostFrontMatter.attributes.title;

    if (!title) {
      throw Error(`A blog post has no title: ${blogPostFileName}`);
    }

    blogPostReferences.push({
      title,
      url,
    });
  }

  let section = `## Blogposts\n`;
  for (const blogPostReference of blogPostReferences) {
    section += `- [${blogPostReference.title}](${blogPostReference.url})\n`;
  }
  return section;
}

function generateBlogPostUrl(blogPostFileName: string): string {
  const baseName = blogPostFileName.replace(/\.(mdx|md)$/, "");

  const [year, month, day, ...slugParts] = baseName.split("-");
  const slug = slugParts.join("-");

  return `${WASP_BASE_URL}blog/${year}/${month}/${day}/${slug}`;
}

function buildLlmFilesIndexSection(waspVersions: string[]): string {
  const latestWaspVersion = waspVersions[0];

  let llmFilesIndexSection = `## Documentation Maps by Version\n*IMPORTANT:* You should run \`wasp version\` to get the installed Wasp CLI version before choosing the correct link.\n`;
  for (const waspVersion of waspVersions) {
    const waspVersionLabel =
      waspVersion === latestWaspVersion
        ? `${waspVersion} (latest)`
        : waspVersion;
    llmFilesIndexSection += `- [${waspVersionLabel}](${WASP_BASE_URL}llms-${waspVersion}.txt)\n`;
  }
  return llmFilesIndexSection;
}
