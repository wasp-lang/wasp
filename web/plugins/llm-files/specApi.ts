import fs from "fs/promises";
import { globSync } from "glob";
import path from "path";

import type { LlmFile } from "./common";

const SPEC_API_DOCS_PATH = "docs/api/@wasp.sh/spec";
const SPEC_API_FILE_NAME = "spec-api.txt";
const WASP_SPEC_API_BASE_URL = "https://wasp.sh/docs/api/@wasp.sh/spec";

export async function buildSpecApiFile(siteDir: string): Promise<LlmFile> {
  const specApiDocsDir = path.join(siteDir, SPEC_API_DOCS_PATH);
  const markdownFiles = globSync("**/*.md", {
    cwd: specApiDocsDir,
    nodir: true,
  }).sort(sortMarkdownFiles);

  if (markdownFiles.length === 0) {
    throw new Error(
      `No generated Spec API Markdown files found in ${specApiDocsDir}`,
    );
  }

  const sections = await Promise.all(
    markdownFiles.map(async (relativePath) => {
      const rawContent = await fs.readFile(
        path.join(specApiDocsDir, relativePath),
        "utf8",
      );

      return [
        `Source: ${getSpecApiPageUrl(relativePath)}`,
        "",
        rewriteLocalMarkdownLinks(rawContent, relativePath),
      ].join("\n");
    }),
  );

  const header = [
    "# Wasp Spec API",
    "",
    "> API reference for @wasp.sh/spec, the TypeScript package used to define Wasp apps in main.wasp.ts.",
  ].join("\n");

  return {
    fileName: SPEC_API_FILE_NAME,
    content: [header, ...sections].join("\n\n---\n\n"),
  };
}

function rewriteLocalMarkdownLinks(
  content: string,
  sourcePath: string,
): string {
  return content.replace(
    /\[([^\]]+)\]\(([^)]+\.md(?:#[^)]+)?)\)/g,
    (match, label, target) => {
      if (target.startsWith("http://") || target.startsWith("https://")) {
        return match;
      }

      const { filePath, hash } = resolveMarkdownTarget(sourcePath, target);
      return `[${label}](${getSpecApiPageUrl(filePath, hash)})`;
    },
  );
}

function resolveMarkdownTarget(
  sourcePath: string,
  target: string,
): { filePath: string; hash: string } {
  const [targetPath, rawHash = ""] = target.split("#");
  return {
    filePath: path
      .normalize(path.join(path.dirname(sourcePath), targetPath))
      .replace(/\\/g, "/"),
    hash: rawHash ? `#${rawHash}` : "",
  };
}

function getSpecApiPageUrl(relativePath: string, hash = ""): string {
  const routePath = relativePath
    .replace(/\.md$/, "")
    .replace(/(^|\/)index$/, "");
  return routePath
    ? `${WASP_SPEC_API_BASE_URL}/${routePath}${hash}`
    : `${WASP_SPEC_API_BASE_URL}${hash}`;
}

function sortMarkdownFiles(a: string, b: string): number {
  if (a === b) return 0;
  if (a === "index.md") return -1;
  if (b === "index.md") return 1;
  return a.localeCompare(b);
}
