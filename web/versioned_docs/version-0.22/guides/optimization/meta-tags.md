---
title: SEO metadata
comments: true
last_checked_with_versions:
  Wasp: "0.21"
---

# Adding SEO metadata to your Wasp app

This guide shows you how to set up meta tags for your Wasp application to improve SEO and enable rich previews when your app is shared on platforms like Slack, X, or Discord.

## How to add `<meta>` tags

### Setting metadata for every page

You can add meta tags to your application using the `head` property in your `app` declaration. These tags will be included in the `<head>` section of your HTML.

```wasp title="main.wasp"
app MyApp {
  wasp: {
    version: "^0.21.0"
  },
  title: "My App",
  head: [
    "<link rel='icon' href='/favicon.ico' />",
    "<meta name='description' content='Your apps main description and features.' />",
    "<meta name='author' content='Your (App) Name' />",
    "<meta name='keywords' content='saas, solution, product, app, service' />",

    // Open Graph tags for social media previews
    "<meta property='og:type' content='website' />",
    "<meta property='og:title' content='Your App Name' />",
    "<meta property='og:site_name' content='Your App Name' />",
    "<meta property='og:url' content='https://your-app.com' />",
    "<meta property='og:description' content='Your apps main description and features.' />",
    "<meta property='og:image' content='https://your-app.com/public-banner.webp' />",

    // Twitter Card tags
    "<meta name='twitter:image' content='https://your-app.com/public-banner.webp' />",
    "<meta name='twitter:image:width' content='800' />",
    "<meta name='twitter:image:height' content='400' />",
    "<meta name='twitter:card' content='summary_large_image' />",
  ],
}
```

### Setting metadata for a specific page

You can use [React's support for `<meta>` tags](https://react.dev/reference/react-dom/components/meta) within components to set metadata for specific pages. This allows you to customize the metadata based on the content of each page. These tags will only be included in the page when the component is being shown, so it's better to add them to the top-most level if possible.

```tsx title="src/pages/HomePage.tsx"
export function HomePage() {
  return (
    <>
      <head>
        <meta
          name="description"
          content="Home page description specific to this page."
        />
        <meta property="og:title" content="Home Page - My App" />
        <meta
          property="og:description"
          content="Description for the home page."
        />
        <meta
          property="og:image"
          content="https://your-app.com/home-page-banner.webp"
        />
        <meta name="twitter:card" content="summary_large_image" />
      </head>
      <div>
        <h1>Welcome to the Home Page</h1>
        {/* The rest of the page content */}
      </div>
    </>
  );
}
```

## Recommended `<meta>` Tags

### Basic SEO tags

- `description`: A brief description of your app (appears in search results)
- `author`: The creator or company name
- `keywords`: Relevant keywords for search engines

### Open Graph tags

This is the most common standard used by social media platforms (e.g. Facebook, LinkedIn, Slack, Discord, and more) to generate rich link previews.

- `og:type`: Usually "website" for web apps
- `og:title`: The title shown in previews
- `og:site_name`: Your app/site name
- `og:url`: The canonical URL
- `og:description`: Description for the preview
- `og:image`: Preview image URL

You can check [Open Graph tag guidelines](https://ogp.me/) for more information on how this information is used.

### X Card tags

This is used by X (formerly Twitter) to create rich link previews.

- `twitter:card`: Use "summary_large_image" for large image previews
- `twitter:image`: Image URL for Twitter previews
- `twitter:image:width`: Image width in pixels
- `twitter:image:height`: Image height in pixels

You can check [X's guidelines](https://developer.x.com/en/docs/x-for-websites/cards/overview/markup) for more information on how this information is used.

## Best practices for images

1. Use your client app's absolute URL (including `https://`) for your preview images.
2. Check the recommended dimensions for each platform's images in their documentation.
3. Keep important content centered (some platforms crop differently).
4. Use WebP or PNG format for best quality.
5. Place your image [in the `public/` folder](https://wasp.sh/docs/project/static-assets#the-public-directory).

## Testing your metadata

After deploying, you can verify your meta tags using these tools:

- [Google Tag Assistant](https://tagassistant.google.com/)
- [Facebook Sharing Debugger](https://developers.facebook.com/tools/debug/)
- [X Card Validator](https://cards-dev.x.com/validator)
- [LinkedIn Post Inspector](https://www.linkedin.com/post-inspector/)
