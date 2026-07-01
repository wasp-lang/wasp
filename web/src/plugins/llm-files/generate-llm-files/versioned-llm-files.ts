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
  loadedVersion: LoadedVersion,
): Promise<void> {
  const waspVersion = loadedVersion.versionName;
  console.log(`Processing Wasp version ${waspVersion}:`);

  const markdownDocsIndex = await buildLlmFilesMarkdownDocsIndex(
    context,
    loadedVersion,
  );

  await generateVersionedLlmsTxt(context, waspVersion, markdownDocsIndex);
  console.log(`- Generated: llms-${waspVersion}.txt`);

  const llmsFullTxtContent = buildLlmsFullTxtContent(markdownDocsIndex);

  await generateVersionedLlmsFullTxt(context, waspVersion, llmsFullTxtContent);
  console.log(`- Generated: llms-full-${waspVersion}.txt`);

  if (waspVersion === context.latestWaspVersion) {
    await generateLatestVersionLlmsFullTxt(
      context,
      waspVersion,
      llmsFullTxtContent,
    );
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
  waspVersion: string,
  markdownDocsIndex: LlmFilesMarkdownDocsIndex,
): Promise<void> {
  const lines: string[] = [`# Wasp ${waspVersion} Documentation`, ""];
  for (const section of markdownDocsIndex.sections) {
    lines.push(`## ${section.title}`);
    buildLlmsTxtBody(lines, section.items, 0);
    lines.push("");
  }

  const absPath = path.join(context.outDir, `llms-${waspVersion}.txt`);
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
      const headingHashes = "#".repeat(3 + depth);
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
 * It includes the content of almost all docs routes (`/docs/*`)  for that Wasp version.
 *
 * @see {@link buildLlmFilesMarkdownDocsIndex} for more details.
 */
async function generateVersionedLlmsFullTxt(
  context: LlmFilesContext,
  waspVersion: string,
  llmsFullTxtContent: string,
): Promise<void> {
  const content =
    buildFullDocsHeader(waspVersion) +
    LLMS_FULL_TXT_HEADER_DIVIDER +
    llmsFullTxtContent;
  const absPath = path.join(context.outDir, `llms-full-${waspVersion}.txt`);

  await fs.writeFile(absPath, content, "utf8");
}

/**
 * Generates a `llms-full.txt` file.
 * It includes the content of almost all docs routes (`/docs/*`)  for the latest Wasp version.
 *
 * @see {@link buildLlmFilesMarkdownDocsIndex} for more details.
 */
async function generateLatestVersionLlmsFullTxt(
  context: LlmFilesContext,
  waspVersion: string,
  llmsFullTxtContent: string,
): Promise<void> {
  const content =
    buildLatestVersionFullDocsHeader(context, waspVersion) +
    LLMS_FULL_TXT_HEADER_DIVIDER +
    llmsFullTxtContent;
  const absPath = path.join(context.outDir, `llms-full.txt`);

  await fs.writeFile(absPath, content, "utf8");
}

/**
 * `llms-full.txt` also includes an index to all other `llms-full-{waspVersion}.txt` variants.
 * This is because LLMs might default to this file even if their user has an outdated
 * Wasp verison (if they skipped the `llms.txt` index).
 */
function buildLatestVersionFullDocsHeader(
  context: LlmFilesContext,
  waspVersion: string,
): string {
  return [
    buildFullDocsHeader(waspVersion),
    "This is the full documentation for the latest version of Wasp.\nFor other versions, see the links below.\n",
    buildFullDocsIndexSection(context),
  ].join("\n");
}

function buildFullDocsHeader(waspVersion: string): string {
  return `# Wasp ${waspVersion} Full Documentation\n`;
}

function buildFullDocsIndexSection(context: LlmFilesContext): string {
  const { baseUrl } = context;
  const waspVersions = context.loadedVersions.map(
    (version) => version.versionName,
  );
  const latestWaspVersion = waspVersions[0];
  let section = `## Full Documentation by Version\n`;
  section += `- [latest (currently ${latestWaspVersion})](${baseUrl}/llms-full.txt)\n`;
  for (const waspVersion of waspVersions) {
    section += `- [${waspVersion}](${baseUrl}/llms-full-${waspVersion}.txt)\n`;
  }
  return section;
}
