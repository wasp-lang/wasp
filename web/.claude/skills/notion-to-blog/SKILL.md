---
name: notion-to-blog
description: Transfer a blog post from Notion to the Wasp blog. Fetches content, downloads and optimizes images, and creates a properly formatted MDX file.
argument-hint: "[notion-page-url]"
---

## Transfer a Notion page to the Wasp blog

Given a Notion page URL, convert it into an MDX blog post for the Wasp website at `web/blog/`.

### Step 1: Fetch the Notion page

1. Extract the page ID from the URL (the UUID at the end, formatted with dashes: `xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx`).
2. Use `mcp__notion__API-retrieve-a-page` to get page metadata (title, properties).
3. Use `mcp__notion__API-get-block-children` to fetch all content blocks. Notion paginates at 100 blocks — check `has_more` and `next_cursor` to fetch all pages.

### Step 2: Parse the content

The Notion page may contain WIP/working notes before and after the actual blog post. Ask the user where the actual post starts and ends if unclear. Typically:
- The post starts at the first `heading_1` block
- WIP sections are often marked with a `## WIP` heading or appear after a `divider` near the end

Use a Python script (via Bash) to parse the JSON block data because it can be very large (50K+ tokens). The script should:
- Extract blocks in the post range
- Convert each block to markdown based on its type:
  - `heading_1/2/3` → `# / ## / ###`
  - `paragraph` → plain text with formatting
  - `bulleted_list_item` → `- `
  - `numbered_list_item` → `1. `
  - `code` → fenced code block with language
  - `image` → collect URL and caption
  - `quote` → `> `
  - `callout` → `> {emoji} `
  - `divider` → `---`
  - `table_of_contents` → skip (Docusaurus handles this)
- Preserve rich text formatting: bold (`**`), italic (`*`), code (`` ` ``), strikethrough (`~~`), links (`[text](url)`)
- Collect all image URLs and captions separately
- Rewrite in-page Notion links as heading anchors (see below)

#### Rewriting in-page Notion links

Notion links to another block on the same page look like `https://app.notion.com/p/<page-id>#<block-id>`, `https://www.notion.so/<slug>-<page-id>#<block-id>`, or `/p/<page-id>?pvs=25#<block-id>`. They break once published, so the script should convert each one into a Docusaurus heading anchor:

1. Build a map of every block ID in the post range (dashes removed, lowercase) to the heading it belongs to: the block itself if it's a `heading_1/2/3`, otherwise the closest heading above it.
2. Turn each heading's final text into a slug the way Docusaurus does. It uses `github-slugger`, which is in `web/node_modules`, so the easiest way is to call it from Node. The rule is: lowercase the text, drop everything except letters, numbers, spaces, hyphens, and underscores, then replace each space with `-`. For example, `TL;DR` → `tldr` and `Part 1: What to build (MCP vs API vs CLI)` → `part-1-what-to-build-mcp-vs-api-vs-cli`. If two headings get the same slug, the second one gets `-1`, the third `-2`, and so on.
3. Replace the link with `#<slug>`, keeping the link text.
4. Keep a list of the links you had to resolve to the closest heading (because the target block wasn't a heading), plus any Notion links you couldn't resolve, like links to other Notion pages or to blocks outside the post range. Leave those links unchanged.

### Step 3: Download and optimize images

1. Create directory: `web/static/img/blog/YYYY-MM-DD-<slug>/` (same name as the MDX file created in step 4)
2. Download all images using `curl` (Notion URLs are temporary signed S3 URLs — download immediately).
3. Give images descriptive names based on their context/caption.
4. Convert all images to WebP using `cwebp -q 85` for smaller file sizes.
5. Remove the original files after conversion.

### Step 4: Create the MDX file

Create `web/blog/YYYY-MM-DD-<slug>.mdx` following these conventions:

**Frontmatter:**
```yaml
---
title: "Post Title"
authors: [authorHandle]  # from web/blog/authors.yml
image: /img/blog/YYYY-MM-DD-<slug>/banner.webp
tags: [wasp, tag2, tag3]  # 3-6 lowercase tags
keywords: ["keyword1", "keyword2", ...]  # 10+ for SEO
description: "SEO summary under 160 chars."
---
```

**Content rules:**
- Add `import { ImgWithCaption } from './components/ImgWithCaption';` after frontmatter
- Add `{/* truncate */}` after the opening hook paragraph (this controls the blog listing excerpt)
- Use `<ImgWithCaption source="..." caption="..." alt="..." />` for ALL images. Omit `caption` prop if there's no caption. When a caption contains double quotes, use single quotes for the prop value instead: `caption='text with "quotes" in it'`
- Make ALL URLs pointing to wasp.sh or wasp-lang.dev **relative** (e.g., `https://wasp.sh/blog/...` → `/blog/...`, `https://wasp.sh/docs/...` → `/docs/...`, `https://wasp-lang.dev/docs/...` → `/docs/...`). This applies to both markdown links and any other references throughout the entire post.
- Keep external URLs (GitHub, Twitter, etc.) as-is

### Step 5: Verify with the user

Show a summary of what was created:
- File path
- Number of images downloaded and total size savings from WebP conversion
- Any decisions made (title choice, skipped sections, etc.)
- In-page links: how many were rewritten to heading anchors, which ones point to the closest heading instead of the exact block (show the link text and the chosen heading), and any Notion links left unresolved
