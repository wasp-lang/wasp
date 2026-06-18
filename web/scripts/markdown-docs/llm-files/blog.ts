import fm from "front-matter";
import { globSync } from "glob";
import path from "path";

import { readFileSync } from "fs";
import { getSiteRoot } from "../site-root";
import { WASP_BASE_URL } from "./constants";

const SITE_ROOT = getSiteRoot();
const BLOG_DIR = path.join(SITE_ROOT, "blog");

interface BlogPostReference {
  title: string;
  url: string;
}

/**
 * Creates a blog posts index section.
 *
 * @example
 * Rendered output:
 * ```md
 * ## Blogposts
 * - [Wasp Launch Week #12 - TS Spec aka MeTSamorphosis 🐛🦋](https://wasp.sh/blog/2026/06/05/wasp-launch-week-12-ts-spec)
 * - [How To Build A SaaS with AI. Without Getting Stuck.](https://wasp.sh/blog/2026/06/03/build-a-saas-with-ai-without-getting-stuck)
 * ...
 * ```
 */
export async function buildBlogPostsIndexSection(): Promise<string> {
  const blogPostsFileNames = globSync("*.{md,mdx}", {
    cwd: BLOG_DIR,
    nodir: true,
    ignore: ["_*.md", "_*.mdx", "CLAUDE.md"], // Ignore markdown partials.
  });

  if (blogPostsFileNames.length === 0) {
    return "";
  }
  // Assumes we use the `YYYY-MM-DD-slug.md(x)` naming convention.
  blogPostsFileNames.sort((a, b) => b.localeCompare(a));

  const blogPostReferences: BlogPostReference[] = [];
  for (const blogPostFileName of blogPostsFileNames) {
    const url = generateBlogPostUrl(blogPostFileName);

    const blogPostAbsFilePath = path.join(BLOG_DIR, blogPostFileName);
    const blogPostFrontMatter = fm<{ [key: string]: string | undefined }>(
      readFileSync(blogPostAbsFilePath, "utf8"),
    );
    const title = blogPostFrontMatter.attributes.title;

    if (!title) {
      throw Error(`A blog post has no title: ${blogPostFileName}`);
    }

    blogPostReferences.push({
      title,
      url,
    });
  }

  let section = `## Blogposts\n`;
  for (const blogPostReference of blogPostReferences) {
    section += `- [${blogPostReference.title}](${blogPostReference.url})\n`;
  }
  return section;
}

function generateBlogPostUrl(blogPostFileName: string): string {
  const baseName = blogPostFileName.replace(/\.(mdx|md)$/, "");

  const [year, month, day, ...slugParts] = baseName.split("-");
  const slug = slugParts.join("-");

  return `${WASP_BASE_URL}blog/${year}/${month}/${day}/${slug}`;
}
