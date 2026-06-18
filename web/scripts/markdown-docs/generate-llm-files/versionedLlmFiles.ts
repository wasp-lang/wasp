import fs from "fs/promises";
import path from "path";

import { getSiteRoot } from "../site-root";
import { WASP_BASE_URL } from "./constants";
import { type DocsIndex, type IndexItem, buildDocsIndex } from "./docs-index";

const SITE_ROOT = getSiteRoot();
const BUILD_DIR = path.join(SITE_ROOT, "build");

/**
 * Generates all `llm-{waspVersion}.txt` and `llm-full-{waspVersion}.txt` files.
 */
export async function generateVersionedLlmFiles(
  waspVersion: string,
  waspVersions: string[],
): Promise<void> {
  console.log(`Processing Wasp version ${waspVersion}:`);

  const docsIndex = await buildDocsIndex(waspVersion);

  await generateVersionedLlmsTxt(waspVersion, docsIndex);
  console.log(`- Generated: llms-${waspVersion}.txt`);

  const llmsFullTxtContent = buildLlmsFullTxtContent(docsIndex);

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
  docsIndex: DocsIndex,
): Promise<void> {
  const lines: string[] = [`# Wasp ${waspVersion} Documentation`, ""];

  for (const section of docsIndex.sections) {
    lines.push(`## ${section.title}`);
    buildLlmsTxtBody(lines, section.items, 0);
    lines.push("");
  }

  const llmsTxtAbsPath = path.join(BUILD_DIR, `llms-${waspVersion}.txt`);
  const llmsTxtContent = lines.join("\n").trimEnd() + "\n";

  await fs.writeFile(llmsTxtAbsPath, llmsTxtContent, "utf8");
}

function buildLlmsTxtBody(
  lines: string[],
  items: IndexItem[],
  depth: number,
): void {
  for (const item of items) {
    if (item.type === "doc") {
      lines.push(`- [${item.title}](${item.docUrl})`);
    } else {
      const headingHashes = "#".repeat(3 + depth);
      lines.push(`${headingHashes} ${item.label}`);
      buildLlmsTxtBody(lines, item.items, depth + 1);
    }
  }
}

function buildLlmsFullTxtContent(docsIndex: DocsIndex): string {
  let fullDocsBody = "";
  for (const section of docsIndex.sections) {
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

async function generateVersionedLlmsFullTxt(
  waspVersion: string,
  llmsFullTxtContent: string,
): Promise<void> {
  const content = buildFullDocsHeader(waspVersion) + llmsFullTxtContent;
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
    llmsFullTxtContent;
  const absPath = path.join(BUILD_DIR, `llms-full.txt`);
  await fs.writeFile(absPath, content, "utf8");
}

function buildLatestVersionFullDocsHeader(
  waspVersion: string,
  waspVersions: string[],
): string {
  return (
    buildFullDocsHeader(waspVersion) +
    "> This is the full documentation for the latest version of Wasp.\n> For other versions, see the links below.\n\n" +
    buildFullDocsIndexSection(waspVersions) +
    "\n---\n\n"
  );
}

function buildFullDocsHeader(waspVersion: string): string {
  return `# Wasp ${waspVersion} Full Documentation\n\n`;
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
