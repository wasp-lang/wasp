---
title: Meta Tags
comments: true
---

# Meta Tags

This guide shows you how to set up meta tags for your Wasp application to improve SEO and enable rich previews when your app is shared on platforms like Slack, Twitter, or Discord.

## Setting Meta Tags in main.wasp

You can add meta tags to your application using the `head` property in your `app` declaration. These tags will be included in the `<head>` section of your HTML.

```wasp title="main.wasp"
app MyApp {
  wasp: {
    version: "^0.15.0"
  },
  title: "My App",
  head: [
    "<link rel='icon' href='/favicon.ico' />",
    "<meta charset='utf-8' />",
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

## Recommended Meta Tags

### Basic SEO Tags

- **description**: A brief description of your app (appears in search results)
- **author**: The creator or company name
- **keywords**: Relevant keywords for search engines

### Open Graph Tags (for Facebook, LinkedIn, Slack, Discord)

- **og:type**: Usually "website" for web apps
- **og:title**: The title shown in previews
- **og:site_name**: Your app/site name
- **og:url**: The canonical URL
- **og:description**: Description for the preview
- **og:image**: Preview image URL (recommended: 1200x630px)

### Twitter Card Tags

- **twitter:card**: Use "summary_large_image" for large image previews
- **twitter:image**: Image URL for Twitter previews
- **twitter:image:width**: Image width in pixels
- **twitter:image:height**: Image height in pixels

## Preview Image Best Practices

1. Use an absolute URL (including `https://`) for your preview images
2. Recommended dimensions: 1200x630 pixels for Open Graph, 800x400 for Twitter
3. Keep important content centered (some platforms crop differently)
4. Use WebP or PNG format for best quality
5. Place your image in the `public/` folder

## Limitations

:::note
The `head` property in Wasp currently only sets meta tags for the main page. Support for setting meta tags on individual pages is planned. See the [GitHub issue](https://github.com/wasp-lang/wasp/issues/911#issuecomment-2008111015) for updates.
:::

## Testing Your Meta Tags

After deploying, you can verify your meta tags using these tools:

- [Facebook Sharing Debugger](https://developers.facebook.com/tools/debug/)
- [Twitter Card Validator](https://cards-dev.twitter.com/validator)
- [LinkedIn Post Inspector](https://www.linkedin.com/post-inspector/)
