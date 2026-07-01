import type { LlmFilesContext } from "./context";
import { generateLlmsTxtFile } from "./llms-txt";
import { generateVersionedLlmFiles } from "./versioned-llm-files";

/**
 * Generates the following files into the build directory:
 * - llms.txt - index of llms-{waspVersion}.txt, llms-full-{waspVersion}.txt, blog posts, and other resources
 * - llms-{waspVersion}.txt - index of that Wasp version's docs, guides, and API pages
 * - llms-full-{waspVersion}.txt - that Wasp version docs and guides concatenated together
 */
export async function generateLlmFiles(
  context: LlmFilesContext,
): Promise<void> {
  console.log("Starting LLM file generation...");

  await generateLlmsTxtFile(context);
  for (const loadedVersion of context.loadedVersions) {
    await generateVersionedLlmFiles(context, loadedVersion);
  }

  console.log("LLM files generation completed successfully.");
}
