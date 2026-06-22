import fm from "front-matter";
import fs from "fs-extra";
import { globSync } from "glob";
import path from "path";

const BLOG_ROOT = process.cwd();
const PUBLIC_DIR = path.join(BLOG_ROOT, "public");
const DOCS_BASE_DIR = path.join(BLOG_ROOT, "src/content/docs");
const ORDERED_SOURCES = ["index.mdx", "start", "guides", "general"]; // Relative to DOCS_BASE_DIR
const GITHUB_RAW_BASE_URL =
  "https://raw.githubusercontent.com/wasp-lang/open-saas/main/opensaas-sh/blog/";
const OPEN_SAAS_DOCS_BASE_URL = "https://docs.opensaas.sh/";
const LLM_FULL_FILENAME = "llms-full.txt";
const LLM_OVERVIEW_FILENAME = "llms.txt";
const OVERVIEW_FILE = path.join(PUBLIC_DIR, LLM_OVERVIEW_FILENAME);
const FULL_CONCAT_FILE = path.join(PUBLIC_DIR, LLM_FULL_FILENAME);

function cleanContent(content) {
  if (!content) return "";
  let cleaned = content;
  // Remove lines starting with 'import ... from ...;' or just 'import ...'
  cleaned = cleaned.replace(/^import\s+.*(?:from\s+['"].*['"])?;?\s*$/gm, "");
  // Remove lines like {/* TODO: ... */}
  cleaned = cleaned.replace(/^\{\/\*.*\*\/\}\s*$/gm, "");
  // Remove lines like <!-- TODO: ... -->
  cleaned = cleaned.replace(/^<!--.*-->\s*$/gm, "");
  // Remove <Image ... /> tags
  cleaned = cleaned.replace(/<Image[^>]*?\/>/g, "");
  // Remove <HiddenLLMHelper /> tag
  cleaned = cleaned.replace(/<HiddenLLMHelper\s*\/>/g, "");
  // Remove Emojis using specific ranges (less likely to remove digits)
  cleaned = cleaned.replace(
    /[\u{1F600}-\u{1F64F}]|[\u{1F300}-\u{1F5FF}]|[\u{1F680}-\u{1F6FF}]|[\u{1F1E0}-\u{1F1FF}]|[\u{2600}-\u{26FF}]|[\u{2700}-\u{27BF}]|[\u{FE00}-\u{FE0F}]|[\u{1F900}-\u{1F9FF}]|[\u{1FA70}-\u{1FAFF}]/gu,
    "",
  );
  cleaned = cleaned.replace(/\u00A0/g, " ");
  // Remove Box Drawing Characters (single and double line)
  cleaned = cleaned.replace(/[│├└─╔═╗║╚╝]/g, "");
  // Remove more than two line breaks
  cleaned = cleaned.replace(/\n{3,}/g, "\n\n");

  return cleaned.trim();
}

async function generateFiles() {
  console.log("Starting LLM file generation...");

  const introSummary =
    "> Open SaaS is a free, open-source, full-stack starter kit for building SaaS applications quickly. It leverages the [Wasp](https://wasp.sh/docs) framework (React, Node.js, Prisma) and integrates with various services like Stripe/Lemon Squeezy, OpenAI API, AWS S3, and different email providers.";

  const orderedSourceFiles = [];
  const fullConcatenatedFileUrl = OPEN_SAAS_DOCS_BASE_URL + LLM_FULL_FILENAME;
  let overviewContent = `# Open SaaS Documentation for LLMs\n\n${introSummary}\n\n## Full Documentation\n - [View Complete LLM-formatted Open SaaS Documentation](${fullConcatenatedFileUrl})\n\n## Individual documentation sections and guides:\n`;
  let fullConcatContent = "";

  console.log("Gathering source files...");
  for (const sourceItem of ORDERED_SOURCES) {
    const itemPath = path.join(DOCS_BASE_DIR, sourceItem);
    try {
      const stats = await fs.stat(itemPath);
      if (stats.isFile()) {
        orderedSourceFiles.push(itemPath);
        console.log(`  Added file: ${sourceItem}`);
      } else if (stats.isDirectory()) {
        const files = globSync(
          path.join(itemPath, "**/*.{md,mdx}").replace(/\\/g, "/"),
          { nodir: true },
        ).sort();
        orderedSourceFiles.push(...files);
        console.log(
          `  Added ${files.length} files from directory: ${sourceItem}`,
        );
      }
    } catch (error) {
      if (error.code === "ENOENT") {
        console.warn(`Warning: Source item not found: ${itemPath}`);
      } else {
        console.warn(
          `Warning: Could not access source item: ${itemPath} - ${error.message}`,
        );
      }
    }
  }
  console.log(`Total source files found: ${orderedSourceFiles.length}`);

  console.log("Processing files...");
  for (const sourceFilePath of orderedSourceFiles) {
    try {
      const rawContent = await fs.readFile(sourceFilePath, "utf8");
      const { attributes, body } = fm(rawContent);

      const processedContent = cleanContent(body);

      const title =
        attributes.title ||
        path.basename(sourceFilePath, path.extname(sourceFilePath));

      const relativeSourcePathForGithub = path
        .relative(BLOG_ROOT, sourceFilePath)
        .replace(/\\/g, "/");
      const githubRawUrl = GITHUB_RAW_BASE_URL + relativeSourcePathForGithub;
      overviewContent += `- [${title}](${githubRawUrl})\n`;

      fullConcatContent += `# ${title}\n\n${processedContent}\n\n---\n\n`;
    } catch (error) {
      console.error(`Error processing file ${sourceFilePath}:`, error);
    }
  }

  console.log("Writing output files to public/ ...");
  try {
    await fs.writeFile(OVERVIEW_FILE, overviewContent.trim(), "utf8");
    console.log(`Generated overview file: ${OVERVIEW_FILE}`);

    await fs.writeFile(FULL_CONCAT_FILE, fullConcatContent.trim(), "utf8");
    console.log(`Generated full concatenated file: ${FULL_CONCAT_FILE}`);
  } catch (error) {
    console.error("Error writing output files:", error);
  }

  console.log("LLM file generation complete.");
}

generateFiles().catch((error) => {
  console.error("Unhandled error during LLM file generation:", error);
  process.exit(1);
});
