import fs from "fs/promises";
import path from "path";

import type { LllmDocsContext, PostCollection } from "../context";

const LLMS_TXT_INTRO = `# Wasp

> Wasp is a full-stack web framework with batteries included for React, Node.js, and Prisma.
> It handles auth, database, routing, deployment, and more out of the box,
> letting you build production-ready apps faster with less boilerplate.`;

const LLMS_TXT_OTHER_RESOURCES = `## Other Resources
- [Wasp Developer Discord](https://discord.com/invite/rzdnErX)
- [Open SaaS - Free, Open-Source SaaS Boilerplate built on Wasp](https://opensaas.sh)
- [Wasp GitHub](https://github.com/wasp-lang/wasp)
`;

export async function generateLlmsTxtFile(
  context: LllmDocsContext,
): Promise<void> {
  const llmFilesIndexSection = buildLlmFilesIndexSection(context);
  const llmFullFilesSection = `## Full Concatenated Documentation Files
Use the same URL pattern as the versioned documentation maps: ${context.baseUrl}/llms-full-{version}.txt`;
  const postIndexSections = context.postCollections.map(buildPostsIndexSection);

  const llmsTxtContent = [
    LLMS_TXT_INTRO,
    llmFilesIndexSection,
    llmFullFilesSection,
    ...postIndexSections,
    LLMS_TXT_OTHER_RESOURCES,
  ]
    .map((content) => content.trimEnd())
    .join("\n\n");
  const llmsTxtAbsPath = path.join(context.outDir, "llms.txt");

  await fs.writeFile(llmsTxtAbsPath, llmsTxtContent, "utf8");
  console.log("Generated: llms.txt");
}

function buildLlmFilesIndexSection(context: LllmDocsContext): string {
  const waspVersions = context.loadedVersions.map(
    (version) => version.versionName,
  );
  const latestWaspVersion = waspVersions[0];

  let llmFilesIndexSection = `## Documentation Maps by Version\n*IMPORTANT:* You should run \`wasp version\` to get the installed Wasp CLI version before choosing the correct link.\n`;
  for (const waspVersion of waspVersions) {
    const waspVersionLabel =
      waspVersion === latestWaspVersion
        ? `${waspVersion} (latest)`
        : waspVersion;
    llmFilesIndexSection += `- [${waspVersionLabel}](${context.baseUrl}/llms-${waspVersion}.txt)\n`;
  }
  return llmFilesIndexSection;
}

/**
 * Builds a posts index section from a {@link PostCollection}.
 *
 * @example
 * ```md
 * ## Blogposts
 * - [Wasp Launch Week #12 - TS Spec aka MeTSamorphosis 🐛🦋](https://wasp.sh/blog/2026/06/05/wasp-launch-week-12-ts-spec.md)
 * - [How To Build A SaaS with AI. Without Getting Stuck.](https://wasp.sh/blog/2026/06/03/build-a-saas-with-ai-without-getting-stuck.md)
 * ...
 * ```
 */
function buildPostsIndexSection(postCollection: PostCollection): string {
  let section = `## ${postCollection.sectionTitle}\n`;
  for (const post of postCollection.posts) {
    section += `- [${post.title}](${post.url})\n`;
  }
  return section;
}
