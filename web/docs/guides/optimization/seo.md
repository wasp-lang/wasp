---
title: SEO & GEO
comments: true
last_checked_with_versions:
  Wasp: "0.24"
---

import { CardLink } from "@site/src/components/CardLink"

# Optimizing for search and AI crawlers (SEO & GEO)

Search engine optimization (SEO) and generative engine optimization (GEO) are about making your app visible and attractive to search engines, social media platforms, and AI assistants.

## Where to optimize

**In general, we recommend applying the following optimization techniques to your content pages, not your app pages.** Think about which pages you want to be surfaced to users, that provide good information _about_ your app to a wide audience.

- **Content pages** include your landing page, about page, pricing page, and other marketing pages. These are the pages you want to show up in search results and link previews. They usually don't change much often, nor based on who visits them. **SEO is most effective for these.**

- **App pages** include your dashboard, profile page, settings page, a form, chat interface, or any page that shows dynamic content based on user data. These pages should be hidden from other users, as they contain personalized data. They also can't be meaningfully indexed, since their main goal is to be interacted with, not read.

You can use this broad distinction to decide which pages you apply SEO techniques to. For example, you definitely want ChatGPT to be able to read your landing page so it can recommend your site to users, but you definitely _don't_ want Google to recommend some user's specific dashboard page to other users.

## Measuring your SEO {#measuring}

We recommend **first to measure your app** against common industry tools and see where you stand. Then, you can pick the techniques that are most relevant to your app and focus on those. After applying them, measure again to see how much they improved your score, and if there are any new issues to fix.

SEO can come with costs; at a minimum, the cost of development time and maintenance burden. So while there's always room for improvement, it's important to focus on the techniques that will give you the biggest boost. A little improvement can go a long way, and you don't need to get a perfect score to see significant benefits.

:::warning Always run measurements against your production build
Full optimization of the Wasp app only happens on the production build, not the development server. Running Lighthouse or other tools against `wasp start` won't reflect what crawlers and users actually get. Always run it against your production build, either [locally](/docs/deployment/local-testing) or [after deploying](/docs/deployment/intro).
:::

### Lighthouse

[Lighthouse](https://developer.chrome.com/docs/lighthouse/overview) should be the first tool you use to measure your website's readiness for search engines and AI assistants. It will score your page on a number issues and give you a neat list of which ones you need to fix, ordered by importance.

<table>
<tbody>
<tr>
<td>

![Screenshot of a Lighthouse report overview](/img/lighthouse-report-overview.png)

</td>
<td>

![Screenshot of a Lighthouse report's list of issues](/img/lighthouse-report-seo.png)

</td>
</tr>
</tbody>
</table>

There are four main categories of issues (Performance, Accessibility, Best Practices, and SEO), and all of these will be important for how your pages perform in search engines and AI assistants. For example, Google uses page speed as a ranking factor, and AI assistants will be more likely to read and recommend your site if it's accessible.

A straightforward way to run Lighthouse is through the command line:

```sh
# First, build and start the production build of your app:
$ wasp build
$ wasp build start

# While the server is running, open another terminal and run Lighthouse:
$ npx lighthouse http://localhost:3000 --preset=desktop --view

# You can change the URL to a specific page or to point to your deployed app if you want to test that instead.
# Remove the --preset=desktop argument to test the mobile experience instead.
```

It will take a minute to run (you'll see an automated browser window while it runs), and then it will save your report as an HTML file and open it.

Using the Lighthouse [command-line interface](https://github.com/GoogleChrome/lighthouse#using-the-node-cli) as we just did is a good way to run it regularly during development, and let your AI assistant read the report and fix issues. You can also run it directly inside the [Chrome DevTools](https://github.com/GoogleChrome/lighthouse#using-lighthouse-in-chrome-devtools), or through the [online service](https://pagespeed.web.dev/) (but it will only work for deployed apps).

### Search Console

[Google Search Console](https://search.google.com/search-console) is a free service from Google that shows you how your deployed site appears in Google Search, which queries show it, and how many clicks it gets.

It will also show you any issues it finds when crawling your site, and guide you through fixing them. It's a must-have for monitoring your SEO performance and catching any issues early on.

### Single-purpose tools

Some SEO techniques have specific tools available to check if they're implemented correctly. Inside the links for each technique below, you will find a mention to that tool if it exists, and we recommend using it to check your implementation.

## Optimization techniques

Good SEO comes down to a few separate problems. We ordered them here by their effort-impact ratio, so you can focus on the techniques that will give you a bigger boost for less effort first. These are:

- Having [good content](#good-content).
- Telling crawlers what each page is about, with [meta tags](#meta-tags).
- Making your content visible without running JavaScript, with [prerendering](#prerendering).
- Helping crawlers understand your content with [semantic markup](#semantic-markup) and [structured data](#structured-data).
- Making your pages [smaller](#reduce-size).
- Giving crawlers the [standard files](#well-known-files) they look for.

These are general techniques that apply to most websites, but there are many more you can use depending on your specific app and goals. You can start with these, and explore more as needed.

### Good content {#good-content}

It all starts with good content. If your page doesn't have useful, relevant content, no amount of SEO will make it rank well. So make sure your content is high-quality, well-written, and provides value to your users. Spamming keywords, using clickbait titles, or generating unreviewed content by the pound won't help you in the long run, and can even get you penalized by search engines. Focus on creating content that answers your users' questions and solves their problems.

You should read Google's guide on creating good content for more tips on what to focus on when creating content pages, before optimizing them for search engines:

<CardLink
  kind="external"
  to="https://developers.google.com/search/docs/fundamentals/creating-helpful-content"
  title="Creating helpful, reliable, people-first content"
  description="From Google"
/>

### Meta tags {#meta-tags}

Search engines and social platforms read `<meta>` tags from your HTML to decide what to show in results and link previews: the page title, its description, and the preview image.

You can set tags for your whole app through the `head` field of your `app` declaration:

```ts title="main.wasp.ts"
import { app } from "@wasp.sh/spec"

export default app({
  // ...
  title: "My App",
  head: [
    // highlight-start
    "<meta name='description' content='Your apps main description.' />",
    "<meta property='og:title' content='My App' />",
    "<meta property='og:image' content='https://your-app.com/banner.webp' />",
    // highlight-end
  ],
})
```

For tags that change from page to page, or depend on dynamic data, you can render `<meta>` tags directly inside a page component using [React's support for `<meta>` tags](https://react.dev/reference/react-dom/components/meta):

```tsx title="src/pages/ProductPage.tsx" auto-js
export function ProductPage({ productId }) {
  const product = useProduct(productId)

  return (
    <>
      {/* highlight-start */}
      <meta name="description" content={product.description} />
      <meta property="og:title" content={product.name} />
      <meta property="og:image" content={product.imageUrl} />
      {/* highlight-end */}

      <h2>{product.name}</h2>
      <p>{product.description}</p>
      {/* ... */}
    </>
  )
}
```

Read more at our dedicated `<meta>` tags guide:

<CardLink
  kind="guide"
  to="/docs/guides/optimization/meta-tags"
  title="Meta tags"
  description="The full set of recommended tags, image guidelines, and testing tools."
/>

### Prerendering {#prerendering}

By default, Wasp apps are [single-page applications (SPAs)](https://en.wikipedia.org/wiki/Single-page_application), so you get fast navigation and responsive interactions. In an SPA, the HTML files your browser downloads are mostly empty and have just enough to load the JavaScript code that actually powers the app. Browsers execute the JavaScript to show you content.

Most search engines (like Google) can also execute JavaScript when indexing a page too. But some crawlers (and many AI assistants) don't run JavaScript at all. So they see only the empty HTML, not your actual content. As such, they can't answer questions about your website, and they'll mostly ignore it.

![Diagram explaining prerendering in Wasp apps. Two side-by-side scenarios compare how a real browser and an AI assistant handle a page. On the left, 'Without prerender': the browser window shows 'Loading...' and the HTML contains only an empty body with a script tag. A real browser executes the JavaScript and successfully shows the page (green checkmark), but the AI assistant only sees the empty HTML, reading the page content as 'Loading,' and responds 'Hmm... I don't know what this page is about,' marked with a red X. On the right, 'With prerender': the browser window shows 'Welcome to Wasp,' and the HTML contains the actual content inside the body. Both the real browser and the AI assistant succeed (green checkmarks); the AI assistant reads 'Welcome to Wasp,' understands the page describes Wasp, and says it will recommend it to the user.](/img/prerendering-diagram.png)

However, Wasp can **prerender** chosen routes to static HTML at build time, so the content is readable in the initial file even without JavaScript. Browsers will still download and execute the rest of the app, and present the same experience as with a pure SPA, so you get the best of both worlds. You can opt-in per route:

```ts title="main.wasp.ts"
import { app, page, route } from "@wasp.sh/spec"
import { LandingPage } from "./src/LandingPage" with { type: "ref" }

export default app({
  // ...
  decls: [
    route("LandingRoute", "/", page(LandingPage), {
      // highlight-next-line
      prerender: true
    }),
  ],
})
```

Prerendering can't be used on routes with dynamic paths or on auth-required pages. You can read more in our prerendering documentation:

<CardLink
  kind="docs"
  to="/docs/advanced/prerendering"
  title="Prerendering"
  description="How it works, when to use it, and how to avoid hydration mismatches."
/>

### Semantic markup

Most crawlers and screen readers can't see your inside images, so every meaningful image needs a text description through its `alt` attribute. Missing `alt` text is one of the most common issues a Lighthouse SEO audit flags.

```tsx title="src/components/Testimonials.tsx" auto-js
<img
  src={testimonial.avatarSrc}
  // highlight-next-line
  alt={`Profile picture of ${testimonial.name}`}
/>
```

If an image is purely decorative, you can give it an empty `alt=""` so crawlers know to ignore it. But if the image conveys information, like a product photo or a profile picture, the `alt` text should describe that information.

You should also use semantic HTML to help crawlers understand your content. For example, use one `<h1>` per page for the main heading, and use `<h2>`, `<h3>`, etc. for subheadings in order. Most indexers will understand that as your page's subject matter and closely relate it with those terms. You should also use descriptive link text instead of generic phrases like "click here," so crawlers know what the linked page is about.

You can check Semrush's post on semantic HTML to see how it looks and which effects it has on SEO:

<CardLink
  kind="external"
  to="https://www.semrush.com/blog/semantic-html5-guide/"
  title="What Is Semantic HTML? And How to Use It Correctly"
  description="From Semrush"
/>

### Structured data

By default, crawlers and AI engines only read your content as text. That is, your page is a bag of words, and they have to guess what those words mean and how they relate to each other. Structured data is a way to give them more information about your content in a machine-readable format, so they can understand it better and show richer results.

If you've ever looked for a recipe on Google and seen a search result with a star rating, cooking time, and a photo, that's structured data at work. And for a typical SaaS app you can use it e.g. in the pricing page, to help crawlers identify the different plans, their features, and their prices, and show that directly in their search results page.

You can add structured data to your pages in multiple ways, but the most common is with [JSON-LD](https://json-ld.org/), which is a script tag with a specific format:

```tsx title="src/pages/WebApplication.tsx" auto-js
import type { WebApplication, WithContext } from "schema-dts";

export function PricingPage() {
  const pricingPlans = usePricingPlans();

  // highlight-start
  const structuredData: WithContext<WebApplication> = {
    "@context": "https://schema.org",
    "@type": "WebApplication",
    name: "My travel app",
    applicationCategory: "TravelApplication",
    offers: pricingPlans.map((plan) => ({
      "@type": "Offer",
      name: plan.name,
      price: plan.price,
      priceCurrency: "USD",
      description: plan.description,
    })),
  };
  // highlight-end

  return (
    <>
      <h1>Our Pricing Plans</h1>
      <p>Choose the plan that works best for you.</p>
      {/* ... */}

      {/* highlight-start */}
      <script type="application/ld+json">
        {JSON.stringify(structuredData)}
      </script>
      {/* highlight-end */}
    </>
  );
}
```

You can read more about structured data in Google's documentation:

<CardLink
  kind="external"
  to="https://developers.google.com/search/docs/appearance/structured-data/intro-structured-data"
  title="Introduction to structured data markup in Google Search"
  description="From Google"
/>

### Reduce your page size {#reduce-size}

Search engines factor page speed into ranking through [Core Web Vitals](https://web.dev/articles/vitals); and the smaller the page, the faster it loads. A couple of things help the most:

- **Optimize your images.** Serve images at the size they're displayed, and prefer modern formats like WebP or AVIF. And [importing an image from your source code](/project/static-assets.md#importing-an-asset-as-url) lets Vite hash its filename so browsers can cache it aggressively. See Chrome Lighthouse's docs for more tips on image optimization:

  <CardLink
    kind="external"
    to="https://developer.chrome.com/docs/performance/insights/image-delivery"
    title="Improve image delivery"
    description="From Chrome"
  />


- **Lazy-load heavy parts of the page.** Wasp can "split" your components so that their HTML, JS, and CSS code don't get loaded upfront. You can split large or below-the-fold components on demand with `React.lazy`:

  ```tsx title="src/pages/LandingPage.tsx" auto-js
  import { lazy } from "react"

  // highlight-start
  // This component might pull in a large graphing
  // library, and it's not at the top of the page,
  // so we don't load it upfront.
  const InteractiveGraph = lazy(() => import("@src/components/InteractiveGraph"));
  // highlight-end

  export function LandingPage() {
    return (
      <div>
        <h1>Welcome to My App</h1>
        <p>Here's some important information about our app...</p>

        {/* Loaded after the rest of the page. */}
        <Suspense fallback={<p>Loading graph...</p>}>
          <InteractiveGraph />
        </Suspense>
      </div>
    )
  }
  ```

  <CardLink
    kind="external"
    to="https://react.dev/reference/react/lazy"
    title="React.lazy"
    description="From React docs"
  />

### Well-known files {#well-known-files}

Crawlers look for a couple of standard files at the root of your site, for example:

- [A `robots.txt` file](#robots-txt) tells crawlers which paths they may visit.
- [An `llms.txt` file](#llms-txt) that can give instructions to AI assistants about how to interact with your site and which pages to read.

Place these in the [`public` directory](/project/static-assets.md#the-public-directory) at the root of your project. Files there are served as-is from the root path, so `public/robots.txt` becomes available at `https://your-app.com/robots.txt`:

```
.
└── public
    ├── favicon.ico
    ├── robots.txt
    ├── llms.txt
    └── sitemap.xml
```

#### `robots.txt` {#robots-txt}

A minimal `robots.txt` lets crawlers visit everything except the routes you don't want indexed, like your admin, API, and auth pages:

```txt title="public/robots.txt"
User-agent: *
Allow: /
Disallow: /admin/
Disallow: /api/
Disallow: /auth/
```

You can check Google's guide on `robots.txt` for more details and examples:

<CardLink
  kind="external"
  to="https://developers.google.com/search/docs/crawling-indexing/robots/intro"
  title="Introduction to robots.txt"
  description="From Google"
/>

#### `llms.txt` {#llms-txt}

An `llms.txt` can direct AI assistants to the main pages they should look at to learn about your app. LLMs are quite good at understanding Markdown, so it's usually written in that format, but you can use any format you want:

```md title="public/llms.txt"
# MyTravelApp

> MyTravelApp is a web application that helps users plan their trips.

## Docs

- [Features](https://mytravelapp.com/features): What the app can do.
- [Pricing](https://mytravelapp.com/pricing): Plans and prices.
- [API documentation](https://mytravelapp.com/api-docs): How LLMs can interact with the app on a user's behalf.

## Getting started

- [Demo](https://mytravelapp.com/demo): Try the app without signing up.
- [Sign up](https://mytravelapp.com/signup): Create an account.
- [Dashboard](https://mytravelapp.com/dashboard): Where users manage their trips after signing up.

## Optional

- [Blog](https://mytravelapp.com/blog): Travel tips and product updates.
```

You can also treat `llms.txt` as a more comprehensive, "alternative" way of presenting information specifically for AI assistants. In this case, instead of pointing to your pages, you'd write out their content in full, so an assistant can learn everything about your app without rendering and parsing the actual pages:

```md title="public/llms.txt"
# MyTravelApp

MyTravelApp is a web application that helps users plan their trips. It lets
you build day-by-day itineraries, track your budget, and share plans with
travel companions.

## Features

- **Itinerary builder.** Add flights, hotels, and activities to a timeline
  that automatically sorts them by date and warns you about overlaps.
- **Budget tracking.** [...]

## Pricing

MyTravelApp is free for one active trip. The Pro plan is $9/month and unlocks
unlimited trips, offline access, and [...]

## Frequently asked questions

**Can I use MyTravelApp offline?** Yes, Pro users can download trips for
offline access [...]

[...]
```

You can check the `llms.txt` proposal website for more details and examples.

<CardLink
  kind="external"
  to="https://llmstxt.org/"
  title="The /llms.txt file"
  description="From Answer.AI"
/>

### Other techniques

There are many more techniques you can use to optimize your app for search engines and AI assistants. There's a wealth of information online about SEO, but we recommend starting with [Google Search's documentation site](https://developers.google.com/search/docs), which is a complete reference on what they look for in a page, and how to optimize for it. They also added a section on [optimizing for AI assistants](https://developers.google.com/search/docs/fundamentals/ai-optimization-guide).

Their starter guide is a good place to begin:

<CardLink
  kind="external"
  to="https://developers.google.com/search/docs/fundamentals/seo-starter-guide"
  title="SEO Starter Guide"
  description="From Google Search"
/>
