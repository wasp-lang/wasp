// Shared helpers for turning Wasp docs source into agent-friendly Markdown.
// Used by both the per-page `.md` plugin (src/plugins/docs-markdown-files.ts)
// and the llms.txt generator (scripts/generate-llm-files.ts).

const WASP_BASE_URL = "https://wasp.sh";

// Strips MDX-only noise (imports, JSX/HTML comments, our custom <Tag/>
// components) so the output reads as plain Markdown. Does not touch heading
// levels: per-page files keep their original structure, and the concatenated
// llms-full.txt applies bumpHeadings separately.
export function cleanMarkdown(content: string): string {
  if (!content) return "";

  const componentsToReplace = collectTagComponentNames(content);

  let cleaned = content;

  // Replace <CompName ... /> with "CompName!" (e.g. <Internal /> -> Internal!)
  componentsToReplace.forEach((compName) => {
    const tagRegex = new RegExp(`<(?:${compName})(\\s+[^>]*)?\\s*/>`, "g");
    cleaned = cleaned.replace(tagRegex, `${compName}!`);
  });

  // Remove all import statements
  cleaned = cleaned.replace(/^import\s+.*(?:from\s+['"].*['"])?;?\s*$/gm, "");
  // Remove JSX comments like {/* TODO: ... */}
  cleaned = cleaned.replace(/^\{\/\*.*\*\/\}\s*$/gm, "");
  // Remove HTML comments like <!-- TODO: ... -->
  cleaned = cleaned.replace(/^<!--.*-->\s*$/gm, "");
  // Collapse 3+ newlines to 2
  cleaned = cleaned.replace(/\n{3,}/g, "\n\n");

  return cleaned.trim();
}

// Increases every heading level by one (# -> ##, ## -> ###, ...). Used when a
// doc body is nested under a `## <title>` section in the concatenated output.
export function bumpHeadings(markdown: string): string {
  return markdown.replace(/^(#{1,6})\s/gm, "#$1 ");
}

// On-site `.md` URL for a doc, mirroring how Docusaurus resolves doc routes.
// The latest version is served without a version segment (/docs/...), older
// versions under /docs/<version>/... . A frontmatter `slug` (always absolute
// in our docs) overrides the docId-derived path.
export function computeDocMarkdownUrl(
  version: string,
  docId: string,
  slug: string | undefined,
  isLatest: boolean,
): string {
  return `${WASP_BASE_URL}${computeDocPermalink(version, docId, slug, isLatest)}.md`;
}

export function computeDocPermalink(
  version: string,
  docId: string,
  slug: string | undefined,
  isLatest: boolean,
): string {
  const base = isLatest ? "/docs" : `/docs/${version}`;

  if (slug !== undefined) {
    if (slug === "/") return base;
    const normalizedSlug = slug.startsWith("/") ? slug : `/${slug}`;
    return `${base}${normalizedSlug}`;
  }

  return `${base}/${stripCategoryIndexSegment(docId)}`;
}

function collectTagComponentNames(content: string): Set<string> {
  const names = new Set<string>();
  // Imports from '@site/src/components/Tag', e.g.
  //   import MyTag from '...'              -> MyTag
  //   import {Tag1, Tag2 as MyTag2} from   -> Tag1, MyTag2
  const importRegex =
    /^\s*import\s+(.+?)\s+from\s+['"]@site\/src\/components\/Tag['"]\s*;?\s*$/gm;

  for (const importMatch of content.matchAll(importRegex)) {
    for (const name of extractImportedNames(importMatch[1])) {
      names.add(name);
    }
  }
  return names;
}

function extractImportedNames(rawSpecifier: string): string[] {
  const names: string[] = [];
  const specifier = rawSpecifier.trim();
  if (specifier.startsWith("{") && specifier.endsWith("}")) {
    const inner = specifier.substring(1, specifier.length - 1).trim();
    if (inner) {
      inner.split(",").forEach((part) => {
        part = part.trim();
        if (part.includes(" as ")) {
          names.push(part.split(" as ")[1].trim());
        } else {
          names.push(part);
        }
      });
    }
  } else if (specifier) {
    names.push(specifier);
  }
  return names.filter(Boolean);
}

// Docusaurus' category index convention: a doc named `index`, `README`, or the
// same as its parent folder (e.g. auth/entities/entities) becomes that folder's
// index page, so its route drops the last segment.
function stripCategoryIndexSegment(docId: string): string {
  const segments = docId.split("/");
  if (segments.length < 2) return docId;
  const last = segments[segments.length - 1].toLowerCase();
  const parent = segments[segments.length - 2].toLowerCase();
  if (last === "index" || last === "readme" || last === parent) {
    return segments.slice(0, -1).join("/");
  }
  return docId;
}
