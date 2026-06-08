// Taps Docusaurus' own MDX pipeline to emit a clean Markdown copy of every doc.
//
// Placed last in the docs `remarkPlugins`, it sees the tree *after* Docusaurus
// has handled heading IDs, admonitions, HTML comments, link/image transforms,
// and our own remark plugins (auto-js, search-and-replace, ...). It flattens
// that tree to Markdown (without mutating it, so the HTML build is unaffected)
// and stages the result on disk. The `docs-markdown-files` plugin then places
// each staged file at its doc permalink during postBuild.

import type { Root } from "mdast";
import { mkdir, writeFile } from "node:fs/promises";
import path from "node:path";
import type { Plugin } from "unified";

import { treeToMarkdown } from "../llm/render-mdx";

// Staging dir under .docusaurus, keyed by each doc's path relative to the site
// root, so the postBuild plugin can find it again from `doc.source`.
export const STAGE_SUBDIR = path.join(".docusaurus", "wasp-docs-markdown");

export function stagedMarkdownPath(
  siteDir: string,
  sourceAbsPath: string,
): string {
  const relPath = path.relative(siteDir, sourceAbsPath);
  return path.join(siteDir, STAGE_SUBDIR, `${relPath}.md`);
}

const extractDocMarkdown: Plugin<[], Root> = () => async (tree, file) => {
  // Skip partials (`_foo.md`); they're inlined into the docs that import them.
  if (file.basename?.startsWith("_")) return;

  let markdown: string;
  try {
    markdown = treeToMarkdown(tree, file.path);
  } catch (err) {
    // Never break the site build over markdown extraction; postBuild falls back
    // to rendering from source when a staged file is missing.
    console.warn(
      `extract-doc-markdown: skipped ${file.path} (${(err as Error).message})`,
    );
    return;
  }

  const outPath = stagedMarkdownPath(file.cwd, file.path);
  await mkdir(path.dirname(outPath), { recursive: true });
  await writeFile(outPath, markdown, "utf8");
};

export default extractDocMarkdown;
