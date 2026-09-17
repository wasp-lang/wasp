# Wasp blog banner visual style

Source of truth for prompt writing. Palette and assets come from Notion
(**Wasp Brand → Assets**); everything else is distilled from banners that shipped.

## Palette

| Token             | Hex                                              |
| ----------------- | ------------------------------------------------ |
| wasp yellow       | `#f5c842`                                        |
| wasp light yellow | `#fff3cc`                                        |
| wasp dark yellow  | `#d4a930`                                        |
| wasp black        | `#111111`                                        |
| wasp white        | `#f7f5f0`                                        |
| wasp grays        | `#333` `#555` `#777` `#999` `#bbb` `#ddd` `#eee` |

Rules of thumb:

- One dominant ground: either flat **wasp yellow** or **wasp white** (a warm cream, not pure white).
- Wasp black for line work and type. Lines are consistent-weight, rounded caps.
- Accents beyond the palette are allowed but rationed — a muted teal/slate for
  "other people's tech", a single orange for Da Boi's blush.
- No gradients beyond a very soft radial glow behind the focal object. No drop
  shadows with blur; use flat offset shapes if depth is needed.

## Da Boi

The mascot: a round bee, yellow face and abdomen with black bands, tiny black
limbs, white wing nub, permanent yellow ribbed beanie bearing the Wasp `=}` logo
badge, big glossy black eyes, orange cheek blush, simple smile.

- Canonical reference: `assets/da-boi.png` (907×861, transparent background).
- Always pass him as a reference image when he appears — never describe him from
  scratch, he goes off-model fast (wrong band count, missing beanie badge, insect legs).
- He is a bystander, not the subject: bottom-left or bottom-right, roughly
  20–30% of frame height, reacting to the thing the post is about.
- Poses that have worked: pointing with a stick at a diagram, peeking from an
  edge, sitting on top of an object, holding one small prop.
- `assets/poses/` collects accepted banners he appeared in — pass one or two as
  extra references when a run needs tighter character consistency.

## Style catalog

Three of these are offered each run alongside two invented for the article.

1. **Flat-vector explainer** — flat yellow field, cream UI panels and a dark code
   editor, dashed connector lines mapping one thing to another, Da Boi pointing.
   Best for "how X works" posts. Text appears only as UI labels and file names.
2. **Bold headline + hero object** — cream ground, huge black display type across
   the top with wasp-yellow highlight blocks behind key words, one central
   metaphor object below, Da Boi small at the edge. Best for announcements.
3. **Cutaway / peel-back** — a clean surface (browser window, page corner) peeled
   or sliced open to reveal dense machinery inside: boards, wires, gears. Best
   for "what's really going on under the hood" posts.
4. **Isometric system diagram** — isometric slabs, pipes and containers showing an
   architecture, light from upper left, flat shading. Best for infra and deploy posts.
5. **Single hero metaphor** — one large, simple, slightly absurd object centered on
   flat yellow, generous negative space, no text. Best for opinion pieces.
6. **Before / after split** — frame divided vertically, cramped grey mess on the
   left, calm yellow order on the right. Best for migration and comparison posts.

## Composition

- 16:9. Keep the focal subject inside the middle 80% — the blog index card and
  social cards crop the edges.
- Busy detail belongs in one region, not spread evenly; leave a quiet zone.
- Nothing important in the bottom-right 15% if Da Boi is present there.

## Text in banners

- Only styles 1 and 2 carry rendered words. Confirm exact wording with the user
  before generating, and keep it to 3–6 words — image models misspell longer strings.
- Set headline words in a heavy geometric sans, all caps or title case, wasp black.
- Never render the full post title verbatim if it is long; use a compressed phrase.
- Check every generated candidate for misspellings before offering it. A typo is
  an automatic reject, not something to fix in post.

## Known failure modes

- `aspect_ratio` left at the model default (`match_input_image`) silently returns a
  square when Da Boi is a reference. `scripts/generate.ts` pins `16:9`.
- Passing too many references flattens the style; 1–3 is the sweet spot.
- Photoreal or 3D renders read as off-brand — always say "flat vector illustration".
- Da Boi rendered at small scale loses the beanie badge; keep him ≥20% of frame height.
