---
name: reposting
description: Repost Wasp blog articles (MDX) to DEV.to and Medium.
---

# MDX Conversion Script

Converts a Wasp blog MDX file to clean markdown, HTML, and Medium-ready chunks. Output is saved to `.claude/skills/reposting/output/`.

```bash
npx tsx .claude/skills/reposting/mdx-to-devto.ts <path-to-mdx-file> [--publish] [--update <id>] [--upload-videos] [--dry-run]
```

**Output files (always written):**
- `<slug>.md` — clean markdown (for DEV.to)
- `<slug>.html` — HTML (for Medium preview/reference)
- `<slug>-medium-chunks.json` — `{ title: string, chunks: string[] }` pre-split at `<h2>` boundaries, pre-escaped for JS template literals

**Flags:**
- `--publish` — POST as draft to DEV.to (requires `DEVTO_API_KEY`)
- `--update <id>` — PUT to existing DEV.to article (requires `DEVTO_API_KEY`)
- `--upload-videos` — upload local .mp4s to YouTube (unlisted), embed as `{% youtube URL %}` liquid tags
- `--dry-run` — print markdown to stdout (default)

**Env vars:** `DEVTO_API_KEY` (from https://dev.to/settings/extensions), `YOUTUBE_CLIENT_ID` + `YOUTUBE_CLIENT_SECRET` (saved in 1password for the users).

**Notes:**
- `canonical_url` is auto-generated from MDX filename → `wasp.sh/blog/...`
- Without `--upload-videos`, local .mp4 videos become plain links

### YouTube Setup (one-time, human steps)

1. Create Google Cloud project → enable YouTube Data API v3 → create OAuth 2.0 Desktop credentials
2. Add `YOUTUBE_CLIENT_ID` and `YOUTUBE_CLIENT_SECRET` to `~/.zshrc`
3. Run `npx tsx .claude/skills/reposting/youtube-upload.ts --auth` → authorize in browser (select @wasplang channel if prompted)
4. Refresh token stored in `~/.youtube-upload-tokens.json`
5. Check channel: `npx tsx .claude/skills/reposting/youtube-upload.ts --whoami`. Wrong channel? Delete token file and re-auth.

---

# Reposting to Dev.to

Use `--publish` or `--update <id>` flags. Requires `DEVTO_API_KEY`.

---

# Reposting to Medium

Paste HTML into Medium's editor via Chrome DevTools MCP. The user must be logged in to Medium as `info@wasp-lang.dev`.

## Steps

### 1. Convert the MDX article

Run the conversion script (see usage above). If the article has local .mp4 videos, add `--upload-videos`. If YouTube URLs already exist from a prior DEV.to repost, run without `--upload-videos` and replace video `<a>` tags in the HTML with `<p>https://youtu.be/VIDEO_ID</p>` (Medium auto-embeds standalone YouTube URLs).

### 2. Navigate to `https://medium.com/new-story`

### 3. Set the article title

**Do NOT use the `fill` tool** — it puts text into the body paragraph instead. Use `evaluate_script` with a synthetic paste:

```javascript
() => {
  const editor = document.querySelector('[contenteditable]');
  const titleEl = editor.querySelector('h3');
  titleEl.focus();
  const range = document.createRange();
  range.selectNodeContents(titleEl);
  const sel = window.getSelection();
  sel.removeAllRanges();
  sel.addRange(range);
  const dt = new DataTransfer();
  dt.setData('text/plain', 'YOUR TITLE HERE');
  const pasteEvent = new ClipboardEvent('paste', {
    bubbles: true, cancelable: true, clipboardData: dt
  });
  titleEl.dispatchEvent(pasteEvent);
  return 'title set';
}
```

Then press `Enter` to move cursor to the body area.

### 4. Paste the article body

**IMPORTANT:** Do NOT use `document.execCommand` — it bypasses Medium's internal state and causes save errors. Always use synthetic `ClipboardEvent` paste.

1. Read `output/<slug>-medium-chunks.json` — chunks are pre-split and pre-escaped.
2. Click the body paragraph to focus it.
3. For each chunk, use `evaluate_script` with the paste function below. After each paste, click the last paragraph to reposition the cursor.

```javascript
(htmlChunk) => {
  const editor = document.querySelector('[contenteditable]');
  const p = editor.querySelector('p:last-of-type') || editor.querySelector('p');
  p.focus();
  const range = document.createRange();
  range.selectNodeContents(p);
  range.collapse(false);
  const sel = window.getSelection();
  sel.removeAllRanges();
  sel.addRange(range);
  const dt = new DataTransfer();
  dt.setData('text/html', htmlChunk);
  dt.setData('text/plain', '');
  const pasteEvent = new ClipboardEvent('paste', {
    bubbles: true, cancelable: true, clipboardData: dt
  });
  p.dispatchEvent(pasteEvent);
  return 'pasted ' + htmlChunk.length + ' chars';
}
```

4. Wait for "Saved" status after all chunks are pasted.

### 5. Verify

- Snapshot the article to confirm it looks correct and shows "Draft" / "Saved"
- Scroll through to confirm section order matches the original and make any necessary adjustments.

### 6. Before Publishing (human steps)

- Manually add any images/YouTube embeds that didn't auto-embed
- Add the banner image and canonical URL
- Review and publish from Medium's UI

## Known Issues

- **Code block language detection:** Medium sometimes guesses wrong (e.g. "Auto (CSS)" for bash). User can fix manually.
