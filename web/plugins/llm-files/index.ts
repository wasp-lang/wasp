import type { LoadContext, Plugin } from "@docusaurus/types";
import fs from "fs/promises";
import path from "path";

import type { LlmFile } from "./common";
import { buildDocsLlmFiles } from "./docs";
import { buildSpecApiFile } from "./specApi";

export function llmFilesPlugin(context: LoadContext): Plugin<void> {
  return {
    name: "llm-files",
    async postBuild({ outDir }) {
      await writeLlmFiles(outDir, [
        ...(await buildDocsLlmFiles(context.siteDir)),
        await buildSpecApiFile(context.siteDir),
      ]);
      console.log("LLM files generation completed successfully.");
    },
  };
}

async function writeLlmFiles(outDir: string, files: LlmFile[]): Promise<void> {
  await fs.mkdir(outDir, { recursive: true });

  for (const file of files) {
    await fs.writeFile(
      path.join(outDir, file.fileName),
      file.content.trim(),
      "utf8",
    );
    console.log(`Generated: ${file.fileName}`);
  }
}
