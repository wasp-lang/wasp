import fs from "fs/promises";
import { globSync } from "glob";
import path from "path";

import { loadPermalinkMaps, rewriteRawGithubDocLinks } from "../permalinks";
import { htmlToMarkdown } from "./convert";
import { buildDocsFooter } from "./footer";

// Post-build step: turn the rendered HTML for docs, blog, and resources pages
// into Markdown served alongside the HTML. Because `trailingSlash` is false,
// Docusaurus emits `<route>.html` files, so we write `<route>.md` next to each.

const SITE_ROOT = process.cwd();
const BUILD_DIR = path.join(SITE_ROOT, "build");

// Only these route prefixes hold prose worth converting. Everything else
// (homepage, search, 404, sitemap) is skipped. The section landing pages
// themselves (`/docs`, `/blog`, `/resources`) are included too.
const ROUTE_PREFIXES = ["/docs", "/blog", "/resources"];

// Listing, tag, and pagination pages have no single document body.
const SKIP_PATH_PATTERNS = [/\/tags(\/|\.html$)/, /\/page\/\d+/, /\/authors\./];

generateMarkdownFiles().catch((err) => {
  console.error("Failed to generate Markdown files:", err);
  process.exit(1);
});

async function generateMarkdownFiles(): Promise<void> {
  console.log("Generating Markdown files from built HTML...");

  const permalinkMaps = loadPermalinkMaps(SITE_ROOT);
  const htmlFiles = findConvertibleHtmlFiles();
  let written = 0;
  let skipped = 0;

  for (const relativePath of htmlFiles) {
    const absolutePath = path.join(BUILD_DIR, relativePath);
    const html = await fs.readFile(absolutePath, "utf8");
    const result = htmlToMarkdown(html);
    if (!result) {
      skipped++;
      continue;
    }

    const body = rewriteRawGithubDocLinks(result.markdown, permalinkMaps);
    const footer = buildDocsFooter(toRoute(relativePath));
    const markdownPath = absolutePath.replace(/\.html$/, ".md");
    await fs.writeFile(markdownPath, `${body}\n${footer}\n`, "utf8");
    written++;
  }

  console.log(
    `Markdown generation complete: ${written} written, ${skipped} skipped (no content).`,
  );
}

function findConvertibleHtmlFiles(): string[] {
  return globSync("**/*.html", {
    cwd: BUILD_DIR,
    nodir: true,
  }).filter(isConvertibleRoute);
}

function isConvertibleRoute(relativePath: string): boolean {
  const route = toRoute(relativePath);
  const isUnderPrefix = ROUTE_PREFIXES.some(
    (prefix) => route === prefix || route.startsWith(prefix + "/"),
  );
  if (!isUnderPrefix) {
    return false;
  }
  const normalized = "/" + relativePath.replace(/\\/g, "/");
  return !SKIP_PATH_PATTERNS.some((pattern) => pattern.test(normalized));
}

// "docs/tutorial/create.html" -> "/docs/tutorial/create"; "docs.html" -> "/docs".
function toRoute(relativePath: string): string {
  return "/" + relativePath.replace(/\\/g, "/").replace(/\.html$/, "");
}
