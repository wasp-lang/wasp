import type { LoadedVersion } from "@docusaurus/plugin-content-docs";
import fs from "fs/promises";
import path from "path";

import type { LllmDocsContext } from "../context";
import {
  type IndexItem,
  type MarkdownDocsIndex,
  buildMarkdownDocsIndex,
} from "./docs-index";

/**
 * Generates all `llm-{waspVersion}.txt` and `llm-full-{waspVersion}.txt` files.
 */
export async function generateVersionedLlmFiles(
  context: LllmDocsContext,
  loadedVersion: LoadedVersion,
): Promise<void> {
  const waspVersion = loadedVersion.versionName;
  console.log(`Processing Wasp version ${waspVersion}:`);

  const markdownDocsIndex = await buildMarkdownDocsIndex(
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

async function generateVersionedLlmsTxt(
  context: LllmDocsContext,
  waspVersion: string,
  markdownDocsIndex: MarkdownDocsIndex,
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
      lines.push(`\n${headingHashes} ${item.title}`);
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
      body += buildLlmsFullTxtBody(item.items, [...breadcrumb, item.title]);
    }
  }
  return body;
}

const LLMS_FULL_TXT_HEADER_DIVIDER = "\n---\n\n";

async function generateVersionedLlmsFullTxt(
  context: LllmDocsContext,
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

async function generateLatestVersionLlmsFullTxt(
  context: LllmDocsContext,
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

function buildLatestVersionFullDocsHeader(
  context: LllmDocsContext,
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

function buildFullDocsIndexSection(context: LllmDocsContext): string {
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
