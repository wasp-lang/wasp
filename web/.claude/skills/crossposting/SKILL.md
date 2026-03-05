---
name: crossposting
description: Crosspost Wasp blog articles (MDX) to DEV.to and Medium.
---

# MDX Conversion Script

Converts a Wasp blog MDX file to clean markdown, HTML, and Medium-ready chunks. Output is saved to `.claude/skills/crossposting/scripts/output/`.

```bash
npx tsx .claude/skills/crossposting/scripts/convert-mdx.ts <path-to-mdx-file> [--publish-devto] [--update-devto <id>] [--upload-videos]
```

**Output files (always written):**
- `<slug>.md` — clean markdown (for DEV.to)
- `<slug>.html` — HTML (for Medium preview/reference)
- `<slug>-medium-chunks.json` — `{ title: string, chunks: string[] }` pre-split at `<h2>` boundaries, pre-escaped for JS template literals

**Flags:**
- `--publish-devto` — POST as draft to DEV.to (requires `DEVTO_API_KEY`)
- `--update-devto <id>` — PUT to existing DEV.to article (requires `DEVTO_API_KEY`)
- `--upload-videos` — upload local .mp4s to YouTube (unlisted), embed as `{% youtube URL %}` liquid tags

**Env vars:** `DEVTO_API_KEY` (from https://dev.to/settings/extensions), `YOUTUBE_CLIENT_ID` + `YOUTUBE_CLIENT_SECRET` (saved in 1password for the users).

**Notes:**
- `canonical_url` is auto-generated from MDX filename → `wasp.sh/blog/...`
- Without `--upload-videos`, local .mp4 videos become plain links

### YouTube Setup (one-time, human steps)

1. Create Google Cloud project → enable YouTube Data API v3 → create OAuth 2.0 Desktop credentials
2. Add `YOUTUBE_CLIENT_ID` and `YOUTUBE_CLIENT_SECRET` to `~/.zshrc`
3. Run `npx tsx .claude/skills/crossposting/scripts/upload-youtube.ts --auth` → authorize in browser (select @wasplang channel if prompted)
4. Refresh token stored in `~/.youtube-upload-tokens.json`
5. Check channel: `npx tsx .claude/skills/crossposting/scripts/upload-youtube.ts --whoami`. Wrong channel? Delete token file and re-auth.

---

# Crossposting to Dev.to

Use `--publish-devto` or `--update-devto <id>` flags. Requires `DEVTO_API_KEY`.

---

# Crossposting to Medium

Paste HTML into Medium's editor via Chrome DevTools MCP. The user must be logged in to Medium using email login (not google oauth) with `info@wasp-lang.dev`.

## Steps

### 1. Convert the MDX article

Run the conversion script, if it hasn't been run yet (see usage above).

**Images:** Medium does not support webp. Convert webp images to jpg for later manual upload:
```bash
for f in static/img/<slug>/*.webp; do sips -s format jpeg "$f" --out ".claude/skills/crossposting/scripts/output/$(basename "${f%.webp}.jpg")"; done
```

### 2. Navigate to `https://medium.com/new-story`

The human user must login to Medium with the `info@wasp-lang.dev` email and get the OTP sent to your email.

### 3. Set the article title

**Do NOT use the `fill` tool** — it puts text into the body paragraph instead. Use `evaluate_script` with the `setMediumTitle` function from `scripts/medium-helpers.js`.

1. Read `setMediumTitle` from `scripts/medium-helpers.js`.
2. Replace `TITLE_PLACEHOLDER` with the actual title (escape backticks and `${` sequences).
3. Call `evaluate_script` with the resulting function string.

Then press `Enter` to move cursor to the body area.

### 4. Paste the article body

**IMPORTANT:** Do NOT use `document.execCommand` — it bypasses Medium's internal state and causes save errors. Always use synthetic `ClipboardEvent` paste.

**IMPORTANT:** Chunks are already pre-escaped for JS template literals and pre-processed for Medium (YouTube URLs, image placeholders). Inline each chunk string directly into the `pasteMediumChunk` function body where `HTML_CHUNK_PLACEHOLDER` appears. Do NOT base64-encode, store chunks in page variables, or add any intermediate encoding steps.

1. Read `output/<slug>-medium-chunks.json` — chunks are ready to paste as-is.
2. Click the body paragraph to focus it.
3. For each chunk, use `evaluate_script` with the `pasteMediumChunk` function from `scripts/medium-helpers.js`. Replace `HTML_CHUNK_PLACEHOLDER` directly with the chunk string content.
4. Wait for "Saved" status after all chunks are pasted.

### 5. Verify

- Snapshot the article to confirm it looks correct and shows "Draft" / "Saved"
- Scroll through to confirm section order matches the original and make any necessary adjustments.

### 6. Before Publishing (human steps)

- Replace `[IMAGE: filename.jpg]` markers with actual image uploads in Medium's editor
- Add any YouTube embeds that didn't auto-embed (Medium auto-embeds standalone YouTube URLs in `<p>` tags)
- Add the banner image and canonical URL
- Review and publish from Medium's UI
