import fs from "fs/promises";
import path from "path";

import { SITE_ROOT_DIR, WASP_BASE_URL } from "../constants";
import {
  type IndexItem,
  type MarkdownDocsIndex,
  buildMarkdownDocsIndex,
} from "./docs-index";

const BUILD_DIR = path.join(SITE_ROOT_DIR, "build");

/**
 * Generates all `llm-{waspVersion}.txt` and `llm-full-{waspVersion}.txt` files.
 */
export async function generateVersionedLlmFiles(
  waspVersion: string,
  waspVersions: string[],
): Promise<void> {
  console.log(`Processing Wasp version ${waspVersion}:`);

  const markdownDocsIndex = await buildMarkdownDocsIndex(waspVersion);

  await generateVersionedLlmsTxt(waspVersion, markdownDocsIndex);
  console.log(`- Generated: llms-${waspVersion}.txt`);

  const llmsFullTxtContent = buildLlmsFullTxtContent(markdownDocsIndex);

  await generateVersionedLlmsFullTxt(waspVersion, llmsFullTxtContent);
  console.log(`- Generated: llms-full-${waspVersion}.txt`);

  const isLatestWaspVersion = waspVersion === waspVersions[0];
  if (isLatestWaspVersion) {
    await generateLatestVersionLlmsFullTxt(
      waspVersion,
      waspVersions,
      llmsFullTxtContent,
    );
    console.log(`- Generated: llms-full.txt`);
  }
}

async function generateVersionedLlmsTxt(
  waspVersion: string,
  markdownDocsIndex: MarkdownDocsIndex,
): Promise<void> {
  const lines: string[] = [`# Wasp ${waspVersion} Documentation`, ""];
  for (const section of markdownDocsIndex.sections) {
    lines.push(`## ${section.title}`);
    buildLlmsTxtBody(lines, section.items, 0);
    lines.push("");
  }

  const absPath = path.join(BUILD_DIR, `llms-${waspVersion}.txt`);
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
      lines.push(`\n${headingHashes} ${item.label}`);
      buildLlmsTxtBody(lines, item.items, depth + 1);
    }
  }
}

function buildLlmsFullTxtContent(markdownDocsIndex: MarkdownDocsIndex): string {
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
      body += buildLlmsFullTxtBody(item.items, [...breadcrumb, item.label]);
    }
  }
  return body;
}

const LLMS_FULL_TXT_HEADER_DIVIDER = "\n---\n\n";

async function generateVersionedLlmsFullTxt(
  waspVersion: string,
  llmsFullTxtContent: string,
): Promise<void> {
  const content =
    buildFullDocsHeader(waspVersion) +
    LLMS_FULL_TXT_HEADER_DIVIDER +
    llmsFullTxtContent;
  const absPath = path.join(BUILD_DIR, `llms-full-${waspVersion}.txt`);

  await fs.writeFile(absPath, content, "utf8");
}

async function generateLatestVersionLlmsFullTxt(
  waspVersion: string,
  waspVersions: string[],
  llmsFullTxtContent: string,
): Promise<void> {
  const content =
    buildLatestVersionFullDocsHeader(waspVersion, waspVersions) +
    LLMS_FULL_TXT_HEADER_DIVIDER +
    llmsFullTxtContent;
  const absPath = path.join(BUILD_DIR, `llms-full.txt`);

  await fs.writeFile(absPath, content, "utf8");
}

function buildLatestVersionFullDocsHeader(
  waspVersion: string,
  waspVersions: string[],
): string {
  return [
    buildFullDocsHeader(waspVersion),
    "This is the full documentation for the latest version of Wasp.\nFor other versions, see the links below.\n",
    buildFullDocsIndexSection(waspVersions),
  ].join("\n");
}

function buildFullDocsHeader(waspVersion: string): string {
  return `# Wasp ${waspVersion} Full Documentation\n`;
}

function buildFullDocsIndexSection(waspVersions: string[]): string {
  const latestWaspVersion = waspVersions[0];
  let section = `## Full Documentation by Version\n`;
  section += `- [latest (currently ${latestWaspVersion})](${WASP_BASE_URL}/llms-full.txt)\n`;
  for (const waspVersion of waspVersions) {
    section += `- [${waspVersion}](${WASP_BASE_URL}/llms-full-${waspVersion}.txt)\n`;
  }
  return section;
}
