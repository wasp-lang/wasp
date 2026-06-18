import fs from "fs/promises";
import path from "path";

import waspVersions from "../../../versions.json";
import { getSiteRoot } from "../site-root";
import { WASP_BASE_URL } from "./constants";
import { type DocsIndex, buildDocsIndex } from "./docs-index";
import { generateLlmsTxtFile } from "./llmsTxt";

const SITE_ROOT = getSiteRoot();
const BUILD_DIR = path.join(SITE_ROOT, "build");

// llms.txt - index of llms-{version}.txt, llms-full-{version}.txt, blog posts, and other resources
// llms-{version}.txt - index of that version's docs, guides, and API pages
// llms-full-{version}.txt - that version's pages concatenated together
generateLlmFiles().catch((err) => {
  console.error("Failed to generate LLM files:", err);
  process.exit(1);
});

async function generateLlmFiles() {
  console.log("Starting LLM file generation...");

  await generateLlmsTxtFile(waspVersions);
  for (const waspVersion of waspVersions) {
    await generateVersionedLlmFiles(waspVersion, waspVersions);
  }

  console.log("LLM files generation completed successfully.");
}

async function generateVersionedLlmFiles(
  waspVersion: string,
  waspVersions: string[],
): Promise<void> {
  console.log(`Processing version ${waspVersion}...`);

  const docsIndex = await buildDocsIndex(waspVersion);

  await generateVersionedLlmTxt(waspVersion, docsIndex);
  console.log(`  Generated: llms-${waspVersion}.txt`);

  const fullDocsBody = buildFullDocsBody(docsIndex);

  await generateVersionedLlmFullTxt(waspVersion, fullDocsBody);
  console.log(`  Generated: llms-full-${waspVersion}.txt`);

  const isLatestWaspVersion = waspVersion === waspVersions[0];
  if (isLatestWaspVersion) {
    await generateLatestVersionLlmFullTxt(
      waspVersion,
      waspVersions,
      fullDocsBody,
    );
    console.log(`  Generated: llms-full.txt`);
  }
}

async function generateVersionedLlmTxt(
  waspVersion: string,
  docsIndex: DocsIndex,
): Promise<void> {
  const lines: string[] = [`# Wasp ${waspVersion} Documentation`, ""];

  for (const section of docsIndex.sections) {
    lines.push(`## ${section.title}`);
    for (const group of section.groups) {
      if (group.label) {
        lines.push(group.label);
      }
      for (const doc of group.docs) {
        lines.push(`- [${doc.title}](${doc.docUrl})`);
      }
    }
    lines.push("");
  }

  const llmsTxtAbsPath = path.join(BUILD_DIR, `llms-${waspVersion}.txt`);
  await fs.writeFile(llmsTxtAbsPath, lines.join("\n").trimEnd() + "\n", "utf8");
}

function buildFullDocsBody(docsIndex: DocsIndex): string {
  let fullDocsBody = "";
  for (const section of docsIndex.sections) {
    fullDocsBody += `# ${section.title}\n\n`;
    for (const group of section.groups) {
      for (const doc of group.docs) {
        fullDocsBody += `## ${doc.title}\n\n${doc.processedBody}\n\n`;
      }
    }
    fullDocsBody += `------\n\n`;
  }
  return fullDocsBody;
}

async function generateVersionedLlmFullTxt(
  waspVersion: string,
  fullDocsBody: string,
): Promise<void> {
  const content = buildFullDocsHeader(waspVersion) + fullDocsBody;
  const absPath = path.join(BUILD_DIR, `llms-full-${waspVersion}.txt`);
  await fs.writeFile(absPath, content, "utf8");
}

async function generateLatestVersionLlmFullTxt(
  waspVersion: string,
  waspVersions: string[],
  fullDocsBody: string,
): Promise<void> {
  const content =
    buildLatestVersionFullDocsHeader(waspVersion, waspVersions) + fullDocsBody;
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
