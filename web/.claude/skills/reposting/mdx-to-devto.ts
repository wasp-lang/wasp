#!/usr/bin/env npx tsx

/**
 * MDX-to-DEV.to Converter & Publisher
 *
 * Converts a Wasp blog MDX file to DEV.to-compatible markdown and
 * optionally publishes it via the Forem API.
 *
 * Usage:
 *   npx tsx mdx-to-devto.ts <path-to-mdx-file> [--publish] [--update <id>] [--upload-videos] [--dry-run]
 *
 * Environment:
 *   DEVTO_API_KEY — required for --publish and --update
 *   YOUTUBE_CLIENT_ID + YOUTUBE_CLIENT_SECRET — required for --upload-videos
 *
 * Options:
 *   --publish         POST the article as a draft to DEV.to (requires DEVTO_API_KEY)
 *   --update <id>     PUT updated content to an existing DEV.to article by ID (requires DEVTO_API_KEY)
 *   --upload-videos   Upload local .mp4 videos to YouTube and embed them as liquid tags
 *   --dry-run         Print the converted markdown to stdout (default without --publish or --update)
 */

import { readFileSync, existsSync } from "fs";
import { basename, resolve } from "path";
import { parse as parseYaml } from "yaml";
import { uploadVideo } from "./youtube-upload.js";

// ---------------------------------------------------------------------------
// Config
// ---------------------------------------------------------------------------

const WASP_BASE_URL = "https://wasp.sh";
const DISCORD_URL = "https://discord.gg/rzdnErX";
const WASP_INTRO_TEXT =
  "_Wasp is a configuration language (DSL) for building full-stack web apps with less code and best practices that works alongside React and Node.js. We are on a mission to streamline web app development while empowering developers to continue using the power of code and their favorite tools. We are backed by Y Combinator and engineers from Airbnb, Facebook, and Lyft._";

// DEV.to allows max 4 tags
const MAX_TAGS = 4;

// DEV.to organization ID for Wasp (https://dev.to/wasp)
const WASP_ORG_ID = 3369;

// ---------------------------------------------------------------------------
// Types
// ---------------------------------------------------------------------------

type MdConverter = (md: string) => string;

interface Frontmatter {
  title: string;
  authors: string[];
  image?: string;
  tags?: string[];
  keywords?: string[];
  description?: string;
  [key: string]: unknown;
}

interface DevToArticle {
  article: {
    title: string;
    body_markdown: string;
    published: boolean;
    canonical_url: string;
    description?: string;
    tags?: string;
    main_image?: string;
    series?: string | null;
    organization_id?: number;
  };
}

interface ConversionResult {
  markdown: string;
  frontmatter: Frontmatter;
  canonicalUrl: string;
}

// ---------------------------------------------------------------------------
// CLI argument parsing
// ---------------------------------------------------------------------------

function parseArgs(argv: string[]) {
  const args = argv.slice(2);
  let filePath = "";
  let publish = false;
  let dryRun = false;
  let updateId = "";
  let uploadVideos = false;

  for (let i = 0; i < args.length; i++) {
    const arg = args[i];
    if (arg === "--publish") publish = true;
    else if (arg === "--dry-run") dryRun = true;
    else if (arg === "--upload-videos") uploadVideos = true;
    else if (arg === "--update") {
      updateId = args[++i] || "";
      if (!updateId) {
        console.error("Error: --update requires an article ID");
        process.exit(1);
      }
    } else if (!arg.startsWith("--")) {
      filePath = arg;
    }
  }

  if (!filePath) {
    console.error(
      "Usage: npx tsx mdx-to-devto.ts <path-to-mdx-file> [--publish] [--update <id>] [--upload-videos] [--dry-run]"
    );
    process.exit(1);
  }

  return { filePath, publish, dryRun, updateId, uploadVideos };
}

// ---------------------------------------------------------------------------
// Frontmatter parsing
// ---------------------------------------------------------------------------

function parseFrontmatter(content: string): {
  frontmatter: Frontmatter;
  body: string;
} {
  const match = content.match(/^---\n([\s\S]*?)\n---\n([\s\S]*)$/);
  if (!match) {
    throw new Error("Could not parse frontmatter from MDX file");
  }

  const fm = parseYaml(match[1]) as Frontmatter;
  return { frontmatter: fm, body: match[2] };
}

// ---------------------------------------------------------------------------
// Build canonical URL from filename
// ---------------------------------------------------------------------------

function buildCanonicalUrl(filePath: string): string {
  const filename = basename(filePath, ".mdx");
  // Filename format: YYYY-MM-DD-slug
  const match = filename.match(/^(\d{4})-(\d{2})-(\d{2})-(.+)$/);
  if (!match) {
    throw new Error(`Unexpected filename format: ${filename}`);
  }
  const [, year, month, day, slug] = match;
  return `${WASP_BASE_URL}/blog/${year}/${month}/${day}/${slug}`;
}

// ---------------------------------------------------------------------------
// MDX → Markdown converters
// ---------------------------------------------------------------------------

function stripMdxComments(md: string): string {
  md = md.replace(/\{\/\*\s*truncate\s*\*\/\}/g, "");
  return md.replace(/\{\/\*[\s\S]*?\*\/\}/g, "");
}

function convertImgWithCaption(md: string): string {
  return md.replace(
    /<ImgWithCaption\s+([\s\S]*?)\/>/g,
    (_match, attrs: string) => {
      const alt = extractAttr(attrs, "alt") || "";
      const source = extractAttr(attrs, "source") || "";
      const caption = extractAttr(attrs, "caption") || "";
      const url = makeAbsoluteUrl(source, WASP_BASE_URL);
      let result = `![${alt}](${url})`;
      if (caption) result += `\n*${stripMarkdownFromCaption(caption)}*`;
      return result;
    }
  );
}

function convertVideoWithCaption(videoMap?: Map<string, string>): MdConverter {
  return (md: string) =>
    md.replace(
      /<VideoWithCaption\s+([\s\S]*?)\/>/g,
      (_match, attrs: string) => {
        const source = extractAttr(attrs, "source") || "";
        const alt = extractAttr(attrs, "alt") || "";
        const caption = extractAttr(attrs, "caption") || "";

        const ytId = videoMap?.get(source);
        if (ytId) {
          let result = `{% youtube https://youtu.be/${ytId} %}`;
          if (caption) result += `\n*${stripMarkdownFromCaption(caption)}*`;
          return result;
        }

        const url = makeAbsoluteUrl(source, WASP_BASE_URL);
        const label = caption || alt || "Video";
        return `[${label}](${url})`;
      }
    );
}

function stripSimpleComponents(md: string): string {
  md = md.replace(/<InBlogCta\s*\/>/g, "");
  md = md.replace(/<DiscordLink\s*\/>/g, `[Discord](${DISCORD_URL})`);
  md = md.replace(/<WaspIntro\s*\/>/g, WASP_INTRO_TEXT);
  return md;
}

function convertReactPlayer(videoMap?: Map<string, string>): MdConverter {
  return (md: string) =>
    md.replace(
      /<ReactPlayer[\s\S]*?url=["']([^"']+)["'][\s\S]*?\/>/g,
      (_match, url: string) => {
        const ytId = videoMap?.get(url);
        if (ytId) {
          return `{% youtube https://youtu.be/${ytId} %}`;
        }
        return `[Video](${makeAbsoluteUrl(url, WASP_BASE_URL)})`;
      }
    );
}

function stripTocInline(md: string): string {
  return md.replace(/<TOCInline[\s\S]*?\/>/g, "");
}

function convertYouTubeIframes(md: string): string {
  md = md.replace(
    /<(?:div[^>]*>\s*)?<iframe[\s\S]*?src=["']https?:\/\/www\.youtube\.com\/embed\/([a-zA-Z0-9_-]+)[^"']*["'][\s\S]*?(?:<\/iframe>|\/?>)\s*(?:<\/div>)?/g,
    (_match, videoId: string) => `{% youtube https://youtu.be/${videoId} %}`
  );
  return md.replace(
    /<iframe[\s\S]*?src=["']https?:\/\/www\.youtube\.com\/embed\/([a-zA-Z0-9_-]+)[^"']*["'][\s\S]*?(?:<\/iframe>|\/?>)/g,
    (_match, videoId: string) => `{% youtube https://youtu.be/${videoId} %}`
  );
}

function convertTabsToBlocks(md: string): string {
  return md.replace(
    /<Tabs>\s*([\s\S]*?)<\/Tabs>/g,
    (_match, inner: string) => {
      const items: string[] = [];
      const tabRegex =
        /<TabItem[^>]*label=["']([^"']+)["'][^>]*>([\s\S]*?)<\/TabItem>/g;
      let tabMatch;
      while ((tabMatch = tabRegex.exec(inner)) !== null) {
        items.push(`**${tabMatch[1]}**\n\n${tabMatch[2].trim()}`);
      }
      return items.join("\n\n---\n\n");
    }
  );
}

function convertAdmonitions(md: string): string {
  return md.replace(
    /^:::(tip|note|warning|info|caution|danger)(?:\s+(.+?)|\[(.+?)\])?\s*\n([\s\S]*?)^:::\s*$/gm,
    (_match, type: string, title1: string, title2: string, content: string) => {
      const label = type.toUpperCase();
      const title = title1 || title2 || "";
      const prefix = title ? `**${label}: ${title}**` : `**${label}**`;
      const lines = content.trim().split("\n");
      return `> ${prefix}\n> \n${lines.map((l) => `> ${l}`).join("\n")}`;
    }
  );
}

function convertImgTags(md: string): string {
  // JSX src={useBaseUrl('...')}
  md = md.replace(
    /<img[^>]*alt=["']([^"']*)["'][^>]*src=\{useBaseUrl\(['"]([^'"]+)['"]\)\}[^>]*\/?>/g,
    (_match, alt: string, src: string) =>
      `![${alt}](${makeAbsoluteUrl(src, WASP_BASE_URL)})`
  );
  // Regular src="..." (alt before src)
  md = md.replace(
    /<img[^>]*alt=["']([^"']*)["'][^>]*src=["']([^"']+)["'][^>]*\/?>/g,
    (_match, alt: string, src: string) =>
      `![${alt}](${makeAbsoluteUrl(src, WASP_BASE_URL)})`
  );
  // src before alt
  return md.replace(
    /<img[^>]*src=["']([^"']+)["'][^>]*alt=["']([^"']*)["'][^>]*\/?>/g,
    (_match, src: string, alt: string) =>
      `![${alt}](${makeAbsoluteUrl(src, WASP_BASE_URL)})`
  );
}

function convertFigures(md: string): string {
  return md.replace(
    /<figure[^>]*>\s*([\s\S]*?)<\/figure>/g,
    (_match, inner: string) => {
      let result = inner;
      const captionMatch = result.match(
        /<figcaption[^>]*>([\s\S]*?)<\/figcaption>/
      );
      result = result.replace(/<figcaption[^>]*>[\s\S]*?<\/figcaption>/, "");
      result = result.trim();
      if (captionMatch) {
        result += `\n*${captionMatch[1].trim()}*`;
      }
      return result;
    }
  );
}

function convertLinks(md: string): string {
  // <Link to={useBaseUrl('...')}>text</Link>
  md = md.replace(
    /<Link\s+to=\{useBaseUrl\(['"]([^'"]+)['"]\)\}>([\s\S]*?)<\/Link>/g,
    (_match, url: string, text: string) =>
      `[${text.trim()}](${makeAbsoluteUrl(url, WASP_BASE_URL)})`
  );
  // <Link to="...">text</Link> and <Link to={'...'}>text</Link>
  return md.replace(
    /<Link\s+to=(?:["']|{'")([^"'}]+)(?:["']|"'})[^>]*>([\s\S]*?)<\/Link>/g,
    (_match, url: string, text: string) =>
      `[${text.trim()}](${makeAbsoluteUrl(url, WASP_BASE_URL)})`
  );
}

function convertAnchorTags(md: string): string {
  return md.replace(
    /<a\s+href=["']([^"']+)["'][^>]*>([\s\S]*?)<\/a>/g,
    (_match, url: string, text: string) => {
      const trimmed = text.trim();
      if (trimmed.startsWith("![") || trimmed.startsWith("<img")) {
        return trimmed;
      }
      return `[${trimmed}](${url})`;
    }
  );
}

function stripWrapperHtml(md: string): string {
  md = md.replace(
    /<div\s+(?:className|style)=[^>]+>\s*([\s\S]*?)<\/div>/g,
    (_match, inner: string) => inner.trim()
  );
  md = md.replace(/<div\s[^>]*><\/div>/g, "");
  md = md.replace(/<div\s[^>]*\/>/g, "");
  md = md.replace(
    /<p\s+align=["'][^"']+["']\s*>\s*([\s\S]*?)<\/p>/g,
    (_match, inner: string) => inner.trim()
  );
  return md.replace(/<br\s*\/?>/g, "\n");
}

function normalizeWhitespace(md: string): string {
  md = md.replace(/\n{4,}/g, "\n\n\n");
  return md.trim() + "\n";
}

// ---------------------------------------------------------------------------
// MDX → Markdown conversion
// ---------------------------------------------------------------------------

function convertMdxToMarkdown(body: string, videoMap?: Map<string, string>): string {
  const pipeline: MdConverter[] = [
    stripImportsOutsideCodeFences,
    stripMdxComments,
    convertImgWithCaption,
    convertVideoWithCaption(videoMap),
    stripSimpleComponents,
    convertReactPlayer(videoMap),
    stripTocInline,
    convertYouTubeIframes,
    convertTabsToBlocks,
    convertAdmonitions,
    convertImgTags,
    convertFigures,
    convertLinks,
    convertAnchorTags,
    stripWrapperHtml,
    stripCodeFenceMeta,
    normalizeWhitespace,
  ];

  return pipeline.reduce((md, convert) => convert(md), body);
}

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

/** Extract a named attribute value from a JSX props string */
function extractAttr(attrs: string, name: string): string | null {
  // Try double-quoted first: name="value" (value may contain single quotes)
  const dblRegex = new RegExp(`${name}="([^"]*?)"`);
  const dblMatch = attrs.match(dblRegex);
  if (dblMatch) return dblMatch[1];

  // Then single-quoted: name='value' (value may contain double quotes)
  const sglRegex = new RegExp(`${name}='([^']*?)'`);
  const sglMatch = attrs.match(sglRegex);
  if (sglMatch) return sglMatch[1];

  return null;
}

/** Make a relative image/video path absolute */
function makeAbsoluteUrl(path: string, base: string): string {
  if (path.startsWith("http://") || path.startsWith("https://")) return path;
  const cleanPath = path.startsWith("/") ? path : `/${path}`;
  return `${base}${cleanPath}`;
}

/** Remove backtick formatting from captions for italic rendering */
function stripMarkdownFromCaption(caption: string): string {
  return caption.replace(/`([^`]+)`/g, "$1");
}

/** Strip import lines that are outside of code fences */
function stripImportsOutsideCodeFences(md: string): string {
  const lines = md.split("\n");
  const result: string[] = [];
  let insideCodeFence = false;

  for (const line of lines) {
    if (line.trimStart().startsWith("```")) {
      insideCodeFence = !insideCodeFence;
    }
    if (
      !insideCodeFence &&
      /^\s*import\s+.+\s+from\s+['"]/.test(line)
    ) {
      continue; // skip this import line
    }
    result.push(line);
  }
  return result.join("\n");
}

/** Strip Docusaurus-specific code fence metadata (title=, ref=, auto-js) */
function stripCodeFenceMeta(md: string): string {
  return md.replace(
    /^(```\w+)\s+(?:title=["'][^"']*["']|ref=["'][^"']*["']|auto-js)[\s]*/gm,
    "$1\n"
  );
}

/** Sanitize tags for DEV.to: lowercase, no hyphens, max 4 */
function sanitizeTags(tags: string[]): string {
  return tags
    .slice(0, MAX_TAGS)
    .map((t) => t.toLowerCase().replace(/-/g, ""))
    .join(", ");
}

// ---------------------------------------------------------------------------
// YouTube video upload
// ---------------------------------------------------------------------------

/** Resolve a local video source path (e.g. /img/foo/demo.mp4) to a file on disk */
function resolveVideoPath(source: string): string | null {
  if (source.startsWith("http://") || source.startsWith("https://")) return null;
  const localPath = source.startsWith("/") ? source.slice(1) : source;
  const fullPath = resolve("static", localPath);
  return existsSync(fullPath) ? fullPath : null;
}

/** Upload a single video to YouTube and return the video ID */
async function uploadSingleVideo(videoPath: string, title: string): Promise<string> {
  const result = await uploadVideo(videoPath, { title, privacy: "unlisted" });
  return result.videoId;
}

/** Scan MDX body for local .mp4 video references and return deduplicated source/title pairs */
function collectVideoSources(body: string): Array<{ source: string; title: string }> {
  const results: Array<{ source: string; title: string }> = [];
  const seen = new Set<string>();

  // Collect from <VideoWithCaption>
  const vwcRegex = /<VideoWithCaption\s+([\s\S]*?)\/>/g;
  let match;
  while ((match = vwcRegex.exec(body)) !== null) {
    const source = extractAttr(match[1], "source") || "";
    if (source && source.endsWith(".mp4") && !seen.has(source)) {
      seen.add(source);
      const title = extractAttr(match[1], "alt") || extractAttr(match[1], "caption") || "";
      results.push({ source, title });
    }
  }

  // Collect from <ReactPlayer>
  const rpRegex = /<ReactPlayer[\s\S]*?url=["']([^"']+)["'][\s\S]*?\/>/g;
  while ((match = rpRegex.exec(body)) !== null) {
    const source = match[1];
    if (source && source.endsWith(".mp4") && !seen.has(source)) {
      seen.add(source);
      results.push({ source, title: "" });
    }
  }

  return results;
}

/** Upload local .mp4 videos to YouTube, return source→videoId map */
async function uploadLocalVideos(
  body: string,
  postTitle: string
): Promise<Map<string, string>> {
  const videoMap = new Map<string, string>();
  const sources = collectVideoSources(body);

  for (let i = 0; i < sources.length; i++) {
    const { source, title: sourceTitle } = sources[i];
    const localPath = resolveVideoPath(source);
    if (localPath) {
      const title = sourceTitle || `${postTitle} - Video ${i + 1}`;
      console.log(`Uploading video: ${source}...`);
      const videoId = await uploadSingleVideo(localPath, title);
      videoMap.set(source, videoId);
      console.log(`  → YouTube: https://youtu.be/${videoId}`);
    } else {
      console.warn(`  ⚠ Video file not found locally: static/${source}`);
    }
  }

  if (videoMap.size > 0) {
    console.log(`\nUploaded ${videoMap.size} video(s) to YouTube.\n`);
  } else {
    console.log("No local .mp4 videos found to upload.\n");
  }

  return videoMap;
}

// ---------------------------------------------------------------------------
// DEV.to API
// ---------------------------------------------------------------------------

async function sendToDevTo(
  article: DevToArticle,
  apiKey: string,
  articleId?: string
): Promise<void> {
  const url = articleId
    ? `https://dev.to/api/articles/${articleId}`
    : "https://dev.to/api/articles";
  const method = articleId ? "PUT" : "POST";

  const response = await fetch(url, {
    method,
    headers: {
      "Content-Type": "application/json",
      "api-key": apiKey,
    },
    body: JSON.stringify(article),
  });

  if (!response.ok) {
    const errorBody = await response.text();
    throw new Error(`DEV.to API error ${response.status}: ${errorBody}`);
  }

  const data = (await response.json()) as Record<string, unknown>;
  console.log(`Article ${articleId ? "updated" : "created"} successfully!`);
  console.log(`  ID:  ${data.id}`);
  console.log(`  Status: ${data.published ? "PUBLISHED" : "DRAFT"}`);
  if (!data.published) {
    console.log(`  Dashboard: https://dev.to/dashboard`);
    console.log(`  Note: Draft preview URLs are only available from the DEV.to dashboard.`);
  } else {
    console.log(`  URL: ${data.url}`);
  }
}

// ---------------------------------------------------------------------------
// Main
// ---------------------------------------------------------------------------

async function main() {
  const { filePath, publish, updateId, uploadVideos } = parseArgs(process.argv);

  // Read and parse the MDX file
  const raw = readFileSync(filePath, "utf-8");
  const { frontmatter, body } = parseFrontmatter(raw);

  // Upload local videos to YouTube if requested
  let videoMap: Map<string, string> | undefined;
  if (uploadVideos) {
    videoMap = await uploadLocalVideos(body, frontmatter.title);
  }

  // Convert
  const markdown = convertMdxToMarkdown(body, videoMap);
  const canonicalUrl = buildCanonicalUrl(filePath);

  const result: ConversionResult = { markdown, frontmatter, canonicalUrl };

  if (!publish && !updateId) {
    // Dry-run: print the converted markdown
    console.log("--- CONVERTED MARKDOWN ---\n");
    console.log(result.markdown);
    console.log("\n--- METADATA ---");
    console.log(`Title:         ${frontmatter.title}`);
    console.log(`Canonical URL: ${canonicalUrl}`);
    console.log(`Tags:          ${sanitizeTags(frontmatter.tags || [])}`);
    console.log(`Description:   ${frontmatter.description || "(none)"}`);
    if (frontmatter.image) {
      console.log(
        `Main Image:    ${makeAbsoluteUrl(frontmatter.image, WASP_BASE_URL)}`
      );
    }
    return;
  }

  // Publish or update mode
  const apiKey = process.env.DEVTO_API_KEY;
  if (!apiKey) {
    console.error(
      "Error: DEVTO_API_KEY environment variable is required for --publish and --update"
    );
    process.exit(1);
  }

  const article: DevToArticle = {
    article: {
      title: frontmatter.title,
      body_markdown: result.markdown,
      published: false,
      canonical_url: canonicalUrl,
      description: frontmatter.description,
      tags: sanitizeTags(frontmatter.tags || []),
      main_image: frontmatter.image
        ? makeAbsoluteUrl(frontmatter.image, WASP_BASE_URL)
        : undefined,
      organization_id: WASP_ORG_ID,
    },
  };

  await sendToDevTo(article, apiKey, updateId || undefined);
}

main().catch((err) => {
  console.error("Error:", err instanceof Error ? err.message : err);
  process.exit(1);
});
