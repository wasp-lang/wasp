import type { LoadedVersion } from "@docusaurus/plugin-content-docs";
import fs from "fs/promises";
import path from "path";

import type { LlmFilesContext } from "./context";
import {
  type IndexItem,
  type LlmFilesMarkdownDocsIndex,
  buildLlmFilesMarkdownDocsIndex,
} from "./docs-index";

/**
 * Generates all `llms-{waspVersion}.txt` and `llms-full-{waspVersion}.txt` files.
 */
export async function generateVersionedLlmFiles(
  context: LlmFilesContext,
  waspVersion: LoadedVersion,
): Promise<void> {
  console.log(`Processing Wasp version ${waspVersion.versionName}:`);

  const markdownDocsIndex = buildLlmFilesMarkdownDocsIndex(
    context,
    waspVersion,
  );

  await generateVersionedLlmsTxt(context, waspVersion, markdownDocsIndex);
  console.log(`- Generated: llms-${waspVersion.versionName}.txt`);

  const llmsFullTxtContent = buildLlmsFullTxtContent(markdownDocsIndex);

  await generateVersionedLlmsFullTxt(context, waspVersion, llmsFullTxtContent);
  console.log(`- Generated: llms-full-${waspVersion.versionName}.txt`);

  if (waspVersion.versionName === context.latestWaspVersion.versionName) {
    await generateLatestVersionLlmsFullTxt(context, llmsFullTxtContent);
    console.log(`- Generated: llms-full.txt`);
  }
}

/**
 * Generates a `llms-{waspVersion}.txt` file.
 * It serves as an index to almost all docs routes (`/docs/*`) for that Wasp version.
 *
 * @see {@link buildLlmFilesMarkdownDocsIndex} for more details.
 */
async function generateVersionedLlmsTxt(
  context: LlmFilesContext,
  waspVersion: LoadedVersion,
  markdownDocsIndex: LlmFilesMarkdownDocsIndex,
): Promise<void> {
  const lines: string[] = [`# Wasp ${waspVersion.label} Documentation`, ""];
  for (const section of markdownDocsIndex.sections) {
    lines.push(`## ${section.title}`);
    buildLlmsTxtBody(lines, section.items, 0);
    lines.push("");
  }

  const absPath = path.join(
    context.outDir,
    `llms-${waspVersion.versionName}.txt`,
  );
  const content = lines.join("\n").trimEnd() + "\n";

  await fs.writeFile(absPath, content, "utf8");
}

function buildLlmsTxtBody(
  lines: string[],
  items: IndexItem[],
  depth: number,
): void {
  for (const item of items) {
    if (item.type === "doc") {
      lines.push(`- [${item.title}](${item.url})`);
    } else {
      const headingHashes = "#".repeat(Math.min(3 + depth, 6));
      lines.push(`${headingHashes} ${item.title}`);
      buildLlmsTxtBody(lines, item.items, depth + 1);
    }
  }
}

function buildLlmsFullTxtContent(
  markdownDocsIndex: LlmFilesMarkdownDocsIndex,
): string {
  let fullDocsBody = "";
  for (const section of markdownDocsIndex.sections) {
    fullDocsBody += `# ${section.title}\n\n`;
    fullDocsBody += buildLlmsFullTxtBody(section.items, []);
    fullDocsBody += `------\n\n`;
  }
  return fullDocsBody;
}

function buildLlmsFullTxtBody(
  items: IndexItem[],
  breadcrumb: string[],
): string {
  let body = "";
  for (const item of items) {
    if (item.type === "doc") {
      const heading = [...breadcrumb, item.title].join(" / ");
      body += `## ${heading}\n\n${item.markdown}\n\n`;
    } else {
      body += buildLlmsFullTxtBody(item.items, [...breadcrumb, item.title]);
    }
  }
  return body;
}

const LLMS_FULL_TXT_HEADER_DIVIDER = "\n---\n\n";

/**
 * Generates a `llms-full-{waspVersion}.txt` file.
 * It includes the content of almost all docs routes (`/docs/*`) for that Wasp version.
 *
 * @see {@link buildLlmFilesMarkdownDocsIndex} for more details.
 */
async function generateVersionedLlmsFullTxt(
  context: LlmFilesContext,
  waspVersion: LoadedVersion,
  llmsFullTxtContent: string,
): Promise<void> {
  const content =
    buildFullDocsHeader(waspVersion.label) +
    LLMS_FULL_TXT_HEADER_DIVIDER +
    llmsFullTxtContent;
  const absPath = path.join(
    context.outDir,
    `llms-full-${waspVersion.versionName}.txt`,
  );

  await fs.writeFile(absPath, content, "utf8");
}

/**
 * Generates a `llms-full.txt` file.
 * It includes the content of almost all docs routes (`/docs/*`) for the latest Wasp version.
 *
 * @see {@link buildLlmFilesMarkdownDocsIndex} for more details.
 */
async function generateLatestVersionLlmsFullTxt(
  context: LlmFilesContext,
  llmsFullTxtContent: string,
): Promise<void> {
  const content =
    buildLatestVersionFullDocsHeader(context) +
    LLMS_FULL_TXT_HEADER_DIVIDER +
    llmsFullTxtContent;
  const absPath = path.join(context.outDir, `llms-full.txt`);

  await fs.writeFile(absPath, content, "utf8");
}

/**
 * `llms-full.txt` also includes an index to all other `llms-full-{waspVersion}.txt` variants.
 * This is because LLMs might default to this file even if their user has an outdated
 * Wasp version (if they skipped the `llms.txt` index).
 */
function buildLatestVersionFullDocsHeader(context: LlmFilesContext): string {
  return [
    buildFullDocsHeader(context.latestWaspVersion.label),
    "This is the full documentation for the latest version of Wasp.\nFor other versions, see the links below.\n",
    buildFullDocsIndexSection(context),
  ].join("\n");
}

function buildFullDocsHeader(waspVersionLabel: string): string {
  return `# Wasp ${waspVersionLabel} Full Documentation\n`;
}

function buildFullDocsIndexSection(context: LlmFilesContext): string {
  const { baseUrl } = context;
  let section = `## Full Documentation by Version\n`;
  section += `- [latest (currently ${context.latestWaspVersion.label})](${baseUrl}/llms-full.txt)\n`;
  for (const version of context.loadedVersions) {
    section += `- [${version.label}](${baseUrl}/llms-full-${version.versionName}.txt)\n`;
  }
  return section;
}
