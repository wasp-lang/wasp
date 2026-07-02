---
title: SEO & GEO
---

import { CardLink } from "@site/src/components/CardLink"

Search engine optimization (SEO) and generative engine optimization (GEO) are about making your app visible and attractive to search engines, social media platforms, and AI assistants. This page is a quick overview of what Wasp already handles for you, and the features you'll use to optimize your app.

## What Wasp does for you

When you run `wasp build`, your app is automatically optimized for production. You don't need to configure anything for:

- **Bundling and minification.** Wasp uses Vite to minify your JavaScript and CSS, and split it by page, so browsers only download the code each page needs. Page speed is a ranking factor for search engines.
- **Asset hashing.** [Assets imported from your source code](/project/static-assets.md#importing-an-asset-as-url) get hashed filenames, so browsers can cache them aggressively.
- **Serving standard files.** Crawlers look for standard files at the root of your site, like `robots.txt`, `sitemap.xml`, or `llms.txt`. Put them in [the `public` directory](/project/static-assets.md#the-public-directory) and Wasp serves them as-is from the root path.

## What Wasp gives you tools for

- **Titles and meta tags.** Search engines and social platforms read `<meta>` tags to build search results and link previews. Set app-wide tags with the [`title` and `head` fields](/project/customizing-app.md#adding-additional-lines-to-the-head) of your `app` declaration, and per-page tags by rendering `<meta>` elements [directly in your page components](https://react.dev/reference/react-dom/components/meta).

- **Prerendering.** Wasp apps are single-page applications, and many crawlers and AI assistants don't run JavaScript, so they'd see an empty page. Mark a route with `prerender: true` and [Wasp generates its HTML at build time](/advanced/prerendering.md), making your content readable without JavaScript.

- **Crawlable navigation.** Crawlers discover your pages by following `<a>` tags. [Wasp's `Link` component](/advanced/links.md) renders real, type-checked `<a>` tags, unlike programmatic navigation, which crawlers can't see.

- **Lazy loading.** Split heavy or below-the-fold components out of the initial bundle with [`React.lazy`](https://react.dev/reference/react/lazy), so your pages stay small and fast.

## Learn more

For the full picture, including how to measure your app, where to apply each technique, and recommended tools, read our dedicated guide:

<CardLink
  kind="guide"
  to="/docs/guides/optimization/seo"
  title="SEO & GEO"
  description="Measure and optimize your app for search engines and AI assistants."
/>
