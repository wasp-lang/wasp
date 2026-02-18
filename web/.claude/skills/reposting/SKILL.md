---
name: reposting
description: Repost Wasp blog articles (MDX) to DEV.to via the Forem API.
---

# Reposting to Dev.to

Repost Wasp blog articles (MDX) to DEV.to via the Forem API.

## Usage

run `npx tsx mdx-to-devto.ts <path-to-mdx-file> [--publish] [--update <id>] [--upload-videos] [--dry-run]`

Env vars:
- `DEVTO_API_KEY` — required for `--publish` and `--update`. Get it from https://dev.to/settings/extensions
- `YOUTUBE_CLIENT_ID` + `YOUTUBE_CLIENT_SECRET` — required for `--upload-videos`. Set up via Google Cloud Console (OAuth 2.0 Desktop app credentials with YouTube Data API v3 enabled).

Flags:
- `--publish`         POST the article as a draft to DEV.to
- `--update <id>`     PUT updated content to an existing DEV.to article by ID
- `--upload-videos`   Upload local .mp4 videos to YouTube (as unlisted) and embed them as `{% youtube ID %}` liquid tags
- `--dry-run`         Print the converted markdown to stdout (default without `--publish` or `--update`)

### YouTube Setup (one-time)

1. Create a Google Cloud project and enable YouTube Data API v3
2. Create OAuth 2.0 credentials (Desktop app type)
3. Add `YOUTUBE_CLIENT_ID` and `YOUTUBE_CLIENT_SECRET` to `~/.zshrc`
4. Run `npx tsx .claude/skills/reposting/youtube-upload.ts --auth` and authorize in the browser
5. Now `--upload-videos` will work — the refresh token is stored in `~/.youtube-upload-tokens.json`

### Uploading to a Brand Account channel (e.g. @wasplang)

Videos are uploaded to whichever YouTube channel you select during the `--auth` OAuth flow. If your Google account manages multiple channels (personal + Brand Accounts like @wasplang), Google will show a **channel picker** — select the @wasplang channel there.

- **No Google Cloud Console changes needed** — the same OAuth credentials work for any channel your account manages.
- After auth, the script prints the authenticated channel name so you can confirm it's correct.
- Run `npx tsx .claude/skills/reposting/youtube-upload.ts --whoami` anytime to check which channel the stored tokens belong to.
- If you're on the wrong channel, delete `~/.youtube-upload-tokens.json` and re-run `--auth`, selecting the correct channel this time.

## Notes

- The `canonical_url` is auto-generated from the MDX filename to point back to `wasp.sh/blog/...`.
- Without `--upload-videos`, local `.mp4` videos are converted to plain links. With it, they're uploaded to YouTube as unlisted videos and embedded as liquid tags.
