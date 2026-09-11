---
name: blog-banner
description: Generate a 16:9 banner image for a Wasp blog post using the Replicate API and the nano-banana-2 model. Evaluates the article, proposes 5 styles, then 5 prompts, then generates 2 candidates to choose from. Use when the user wants a banner, hero, or cover image for a blog post.
argument-hint: "[post slug, path, or title]"
---

## Generate a blog banner

Produce `banner.webp` for a Wasp blog post and wire it into the post's frontmatter.

Read `reference/wasp-visual-style.md` before writing any prompt — it holds the
palette, Da Boi's canon, the style catalog and the known failure modes.

All paths below are relative to `web/`. Work in the session scratchpad until the
user accepts an image; nothing enters `static/img/` before then.

### Step 1: Find and evaluate the article

The argument may be a slug (`2026-08-18-flue-react-for-agents`), a path, or a
title fragment. Match it against `blog/*.mdx`; if several match or the argument is
missing, list candidates and ask.

Read the whole post, then write a 3–4 line brief for yourself:

- The **thesis** — the one claim the post is making.
- The **angle** — announcement, tutorial, opinion, showcase, post-mortem?
- The **concrete nouns** the post keeps returning to. These are what the banner
  can actually depict; abstractions are not drawable.
- The **tone** — playful, technical, contrarian?

Derive `<post-slug>` from the filename with the date stripped
(`2026-08-18-flue-react-for-agents` → `flue-react-for-agents`), unless the post
already has an `image:` pointing at an existing `static/img/<dir>/`, in which case
reuse that directory.

### Step 2: Offer 5 styles

Three from the catalog in `reference/wasp-visual-style.md`, picked for fit to the
brief, plus two invented for this specific article. For each give a name, one
sentence of description, and whether it carries rendered text.

Say which one you would pick and why. The user selects, combines, or redirects.

### Step 3: Settle the reference images

Ask **"include Da Boi?"** explicitly every run — it is a per-post call, not a default.

Available references (`image_input` takes up to 14, but 1–3 is the sweet spot):

- **Da Boi** — `assets/da-boi.png`. Required whenever he appears.
- **Style anchors** — one or two shipped banners, e.g.
  `static/img/roll-your-own-file-based-router/banner.webp` (flat-vector explainer)
  or `static/img/flue-open-saas/banner.webp` (bold headline). Use when the chosen
  style is one of the catalog styles.
- **Da Boi poses** — `assets/poses/*.png`, past accepted banners. Use when a run
  keeps drifting off-model.
- **Post images** — anything already under `static/img/<post-slug>/`, e.g. a real
  screenshot to embed in the banner.
- **Logos** — `assets/wasp-logo.png`, or a third-party logo the user points at.

For brand assets not already vendored, pull from Notion: **Wasp Brand → Assets**
(data source `collection://6da45d72-e42f-4d67-9cba-64d51167a910`) holds the color
system, logo and canonical Da Boi. Fetch the page, download the signed S3 URL
immediately — **it expires in 5 minutes** — and rasterize any SVG with
`qlmanage -t -s 1024 -o DIR FILE.svg`.

> If the Notion MCP server is not connected, say so plainly and carry on with the
> vendored assets in `assets/`. Do not block the run on it.

### Step 4: Offer 5 prompts

Write five prompts for the chosen style — same style, genuinely different ideas,
not five rewordings. Each should name the composition, the focal object, where
Da Boi sits (if included), the ground colour, and end with the flat-vector
language from the style reference.

If the style carries text, confirm the exact words with the user before generating.
Keep it to 3–6 words.

The user selects, edits, expands, or asks for another five.

### Step 5: Generate 2 candidates

Write the final prompt to a scratchpad file (avoids shell-quoting damage), then:

```bash
SCRATCH=/path/to/session/scratchpad   # set to the real scratchpad dir

npx tsx .claude/skills/blog-banner/scripts/generate.ts \
  --prompt-file "$SCRATCH/prompt.txt" \
  --out-dir "$SCRATCH/banners" \
  --count 2 \
  --resolution 1K \
  --ref .claude/skills/blog-banner/assets/da-boi.png
```

Never put `<angle-bracket>` placeholders or a bare `$0` inside this file — the
former are parsed as shell redirections and reordered, the latter interpolates
into the skill argument. Use shell variables and spell out `USD`.

The script pins `aspect_ratio: 16:9` and `output_format: png`, uploads local refs
via Replicate's Files API, runs both predictions in parallel and prints a JSON
summary with the written paths and the actual cost.

**Confirm the cost with the user before the first call of a run.** 1K is
`USD 0.067` per image, so a pair is `USD 0.134`. Add `--dry-run` to show exactly what
would be sent without spending anything.

Token resolution is: 1Password (`op read`) → `$REPLICATE_API_TOKEN`. If both are
missing the script prints setup instructions; relay them rather than guessing.

### Step 6: Review and iterate

Open both for the user, then read them yourself and check for misspelled text,
off-model Da Boi and anything cropped by the 16:9 frame:

```bash
open "$SCRATCH/banners/candidate-1.png" "$SCRATCH/banners/candidate-2.png"
```

Offer four moves:

- **Accept one** → step 7.
- **Reroll** — same prompt, two fresh takes.
- **Edit the prompt** — revise and regenerate the pair.
- **Conversational edit** — pass the chosen candidate back as a reference with an
  instruction naming only what changes ("same composition, make Da Boi smaller and
  move him to the left"). Use `--ref <that candidate>` and `--count 1`.

### Step 7: Finalize

1. **Offer a 2K re-run.** 1K at 16:9 returns 1376x768, which already matches the
   smaller shipped Wasp banners, so this is optional - offer it, don't assume it.
   Worth doing when the art has fine detail (small type, dense line work) that
   would benefit from ~2048px. Use the identical prompt and references with
   `--resolution 2K --count 2 --prefix final`. It is a fresh roll, not an upscale,
   so show the results and confirm one still matches what was approved - if
   neither does, reroll or keep the approved 1K image and say so.
2. Convert: `cwebp -q 85 CHOSEN.png -o static/img/POST-SLUG/banner.webp`
   (create the directory if needed), then delete the intermediate PNG.
3. Set `image: /img/<post-slug>/banner.webp` in the post's frontmatter.
4. If Da Boi appeared, copy the accepted banner to
   `.claude/skills/blog-banner/assets/poses/<post-slug>.png`.
5. Append the run to `.claude/skills/blog-banner/log/<post-slug>.md`: date, style
   name, final prompt, reference images, resolution, total spend.
6. Report the file path, its dimensions and size, the frontmatter change, and the
   total cost of the run.
