# "No Lock-In" section - interactive redesign (prototype)

Handoff notes for continuing this work. Read this before touching code.

## What this is

The "No Lock-In" landing-page section (wasp.sh homepage) shipped as a static
version (already merged into `release`: `web/src/components/NoLockIn.tsx`, a
heading + badges + a TypeScript-union pseudo-code block + three bullets).

This branch is for a **follow-up redesign**: making the section interactive and
more distinctive. The prototype lives next to this file:

- `index.html` - a self-contained, clickable prototype. Open it in a browser.
- Live copy (may lag behind the file): https://wasp-nolockin-lab.surge.sh

## The chosen direction: "Pick your target"

We explored 3 variants in a lab; **variant A won**. It replaces the abstract
pseudo-code with an interactive control:

- A grid of **provider tiles** (Fly.io, Railway, Google Cloud, DigitalOcean,
  Hetzner, Self-host/Docker).
- Clicking a tile updates a **terminal** showing the actual command:
  - Fly / Railway (the real one-command targets, flagged "1 cmd"):
    `wasp deploy fly|railway launch ...`
  - Everything else (bring-your-own): `wasp build` -> Dockerfile + static files
    -> `docker build . && deploy`.
- This makes "deploy anywhere / no lock-in" something you can *poke*, and it
  fills the dead space the original layout had on the right.

## Design decisions already made (don't re-litigate)

- **Flat background.** An earlier version had a blueprint construction grid +
  corner ticks - it read as too cartoony/busy. Removed. Keep it flat like the
  rest of the site.
- **Real provider logos, monochrome.** Earlier I invented geometric shapes for
  AWS/GCP - rejected as fake. Current icons are the *real* brand marks rendered
  single-color black so they sit in the neo-brutalist palette instead of
  dragging in six brand colors.
- **AWS was dropped** on purpose: no clean single-icon mark available, and it's
  not a first-class Wasp deploy target. DigitalOcean + Docker cover the "any
  cloud / your own box" story with real logos.
- **Terminal uses Wasp's palette.** Background is `code-bg-purple-dark`
  (`#292435`), prompt/cursor yellow, success `✓` in yellow, text in lavender.
  No green - green is off-brand.
- **Angular open padlock** accent on the heading (sharp corners, no arcs).

## Goal for this branch

Rebuild the prototype as the real `web/src/components/NoLockIn.tsx`:

- Use the site's existing building blocks: `SectionContainer`, `SectionLabel`,
  `CodeHighlight`, `TextLink`, `InlineCode`.
- Use Tailwind `wasp-*` tokens (see `web/tailwind.config.js`), not hardcoded hex.
- Match the merged component's structure/altitude; this replaces it.
- Keep it accessible (tiles are real buttons; terminal updates announced ok).

## Caveats / TODO before shipping publicly

- **Provider logos**: the prototype inlines SVG paths pulled from Simple Icons
  (MIT-licensed icon set). For production, vendor them as proper assets and
  **check each provider's trademark/brand-usage guidelines** before using their
  marks on a public marketing page.
- **Terminal commands are lightly simplified** (`docker build . && deploy` is
  illustrative shorthand, not a literal Wasp command) - do a copy pass, ideally
  with Martin, so the shown commands are accurate.
- Consider whether Hetzner (heavier filled mark) should be swapped for
  Render/Netlify, which are actual Wasp deployment guides.

## Reference command palette (from the prototype)

- `fly`     -> `wasp deploy fly launch my-app my-app-db`
- `railway` -> `wasp deploy railway launch my-app`
- byo (gcp / digitalocean / hetzner / docker) -> `wasp build` -> Dockerfile +
  static files -> ship to `<host>`, `docker build . && deploy`
