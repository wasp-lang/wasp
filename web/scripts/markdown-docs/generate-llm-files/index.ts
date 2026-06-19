import waspVersions from "../../../versions.json";
import { generateLlmsTxtFile } from "./llmsTxt";
import { generateVersionedLlmFiles } from "./versionedLlmFiles";

/**
 * Generates the following files:
 * - llms.txt - index of llms-{waspVersion}.txt, llms-full-{waspVersion}.txt, blog posts, and other resources
 * - llms-{waspVersion}.txt - index of that Wasp version's docs, guides, and API pages
 * - llms-full-{waspVersion}.txt - that Wasp version docs and guides concatenated together
 */

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
