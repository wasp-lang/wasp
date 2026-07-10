import type { LlmFilesContext } from "./context";
import { generateLlmsTxtFile } from "./llms-txt";
import { generateVersionedLlmFiles } from "./versioned-llm-files";

/**
 * Generates all LLM files in the {@link LlmFilesContext.outDir}.
 * This includes:
 * - `llms.txt` - universal index to all other `llms*.txt` files and resources.
 * - `llms-{waspVersion}.txt` - index to that Wasp version's docs.
 * - `llms-full-{waspVersion}.txt` - content of that Wasp version's docs.
 * - `llms-full.txt` - content of the latest Wasp version's docs and an index to other `llms-full-{waspVersion}.txt` files.
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
