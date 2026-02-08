# MUI SSR Example

A minimal Wasp example demonstrating **MUI (Material UI)** with **server-side rendering** and **Emotion CSS-in-JS extraction**.

## What This Demonstrates

- **SSR with MUI**: Two pages (`/` and `/about`) rendered on the server with full MUI styling
- **Emotion CSS-in-JS extraction**: Styles generated during SSR are collected and injected into the HTML `<head>`, eliminating flash of unstyled content (FOUC)
- **SSR styles provider hook**: The `src/ssr/styles.tsx` convention that lets you plug in any CSS-in-JS library for SSR
- **SEO metadata**: Each page exports a `head()` function for SSR-injected `<title>` and meta tags
- **Custom MUI theme**: Typography (Barlow headings + Inter body), color palette, and component overrides

## Key Files

| File | Purpose |
|------|---------|
| `src/ssr/styles.tsx` | **Emotion SSR provider** — creates a cache per request, wraps the tree in `CacheProvider`, and extracts critical CSS after render |
| `src/Root.tsx` | Root component wrapping all pages with `ThemeProvider` + `CssBaseline` |
| `src/theme/` | Custom MUI theme (palette, typography) |
| `src/pages/LandingPage.tsx` | Landing page with Hero, Services grid, and FAQ accordion |
| `src/pages/AboutPage.tsx` | About page with stats and SSR verification section |
| `main.wasp` | App config with `ssr: true` on both pages |

## How SSR Styles Work

1. **Wasp detects** `src/ssr/styles.tsx` and imports `createSsrStylesProvider` during SSR
2. **On each request**, Wasp calls `createSsrStylesProvider()` which returns:
   - `Wrapper` — an Emotion `CacheProvider` that wraps your React tree
   - `extractStyles(html)` — extracts critical CSS from the rendered HTML
3. **Wasp injects** the extracted `<style data-emotion="...">` tags into the HTML `<head>`
4. **The browser** receives fully styled HTML — MUI components look correct on first paint
5. **React hydrates** and Emotion takes over on the client

## Setup

No environment variables needed — this example uses SQLite and has no auth.

## Running

1. Start the app:

```sh
wasp start
```

2. Open `http://localhost:3000` in your browser

## Verify SSR

1. Navigate to any page
2. Right-click and select **View Page Source**
3. You should see:
   - All page content rendered in the HTML (not just an empty `<div id="root">`)
   - `<style data-emotion="css ...">` tags in the `<head>` containing MUI styles
   - No flash of unstyled content on page load

## Adapting for Other CSS-in-JS Libraries

The `src/ssr/styles.tsx` convention is library-agnostic. To use a different library:

1. Create `src/ssr/styles.tsx` exporting `createSsrStylesProvider()`
2. Return `{ Wrapper, extractStyles }` using your library's SSR API
3. Install your library's SSR packages

Examples for styled-components, Stitches, and more are in the [Wasp SSR docs](https://wasp-lang.dev/docs/advanced/ssr).
