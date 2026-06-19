import fm from "front-matter";
import fs from "fs/promises";
import { glob } from "glob";
import path from "path";

import { getSiteRoot } from "../site-root";
import { WASP_BASE_URL } from "./constants";

const SITE_ROOT = getSiteRoot();
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
  const llmFilesIndexSection = buildLlmFilesIndexSection(waspVersions);
  const llmFullFilesSection = `## Full Concatenated Documentation Files
Use the same URL pattern as the versioned documentation maps: ${WASP_BASE_URL}/llms-full-{version}.txt`;
  const blogPostsIndexSection = await buildPostsIndexSection(
    "Blogposts",
    "blog",
  );
  const resourcesIndexSection = await buildPostsIndexSection(
    "Resources",
    "resources",
  );

  const llmsTxtContent = [
    LLMS_TXT_INTRO,
    llmFilesIndexSection,
    llmFullFilesSection,
    blogPostsIndexSection,
    resourcesIndexSection,
    LLMS_TXT_RESOURCES,
  ]
    .map((content) => content.trimEnd())
    .join("\n\n");
  const llmsTxtAbsPath = path.join(BUILD_DIR, "llms.txt");

  await fs.writeFile(llmsTxtAbsPath, llmsTxtContent, "utf8");
  console.log("Generated: llms.txt");
}

interface PostReference {
  title: string;
  url: string;
}

/**
 * Generates a posts index section for a date-based content plugin.
 * Blog and resources share the `YYYY-MM-DD-slug.md(x)` naming and
 * `/<routeBasePath>/YYYY/MM/DD/slug` routing.
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
async function buildPostsIndexSection(
  sectionTitle: string,
  routeBasePath: string,
): Promise<string> {
  const postsDir = path.join(SITE_ROOT, routeBasePath);
  const postsFileNames = await glob("*.{md,mdx}", {
    cwd: postsDir,
    nodir: true,
    ignore: ["_*.md", "_*.mdx", "CLAUDE.md"], // Ignore markdown partials.
  });

  if (postsFileNames.length === 0) {
    return "";
  }
  // Assumes we use the `YYYY-MM-DD-slug.md(x)` naming convention.
  postsFileNames.sort((a, b) => b.localeCompare(a));

  const postReferences: PostReference[] = [];
  for (const postFileName of postsFileNames) {
    const url = generatePostUrl(routeBasePath, postFileName);

    const postAbsFilePath = path.join(postsDir, postFileName);
    const postFrontMatter = fm<{ [key: string]: string | undefined }>(
      await fs.readFile(postAbsFilePath, "utf8"),
    );
    const title = postFrontMatter.attributes.title;

    if (!title) {
      throw Error(`A post has no title: ${postFileName}`);
    }

    postReferences.push({
      title,
      url,
    });
  }

  let section = `## ${sectionTitle}\n`;
  for (const postReference of postReferences) {
    section += `- [${postReference.title}](${postReference.url})\n`;
  }
  return section;
}

function generatePostUrl(routeBasePath: string, postFileName: string): string {
  const baseName = postFileName.replace(/\.(mdx|md)$/, "");

  const [year, month, day, ...slugParts] = baseName.split("-");
  const slug = slugParts.join("-");

  return `${WASP_BASE_URL}/${routeBasePath}/${year}/${month}/${day}/${slug}`;
}

function buildLlmFilesIndexSection(waspVersions: string[]): string {
  const latestWaspVersion = waspVersions[0];

  let llmFilesIndexSection = `## Documentation Maps by Version\n*IMPORTANT:* You should run \`wasp version\` to get the installed Wasp CLI version before choosing the correct link.\n`;
  for (const waspVersion of waspVersions) {
    const waspVersionLabel =
      waspVersion === latestWaspVersion
        ? `${waspVersion} (latest)`
        : waspVersion;
    llmFilesIndexSection += `- [${waspVersionLabel}](${WASP_BASE_URL}/llms-${waspVersion}.txt)\n`;
  }
  return llmFilesIndexSection;
}
