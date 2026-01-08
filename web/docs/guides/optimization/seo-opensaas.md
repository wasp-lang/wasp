---
last_update:
  date: 2025-09-16
title: SEO for OpenSaaS
comments: true
---

# SEO Optimization for OpenSaaS

This guide shows you how to achieve a perfect SEO score with the OpenSaaS template. By following these steps, you can improve your site's discoverability and search engine rankings.

## Initial Assessment

After setting up an OpenSaaS project, an audit using Chrome DevTools Lighthouse typically reveals a base SEO score around 75. Let's optimize this to reach 100%.

## 1. Adding Meta Description

### Issue

The meta description may not be appearing in the DOM because the meta tags in `main.wasp` are commented out by default.

### Solution

Uncomment and configure the meta description section in your `main.wasp` file:

```wasp title="main.wasp"
app OpenSaaS {
  // ... other configurations ...

  head: [
    "<meta property='og:type' content='website' />",
    "<meta property='og:title' content='My Open SaaS App' />",
    "<meta property='og:url' content='https://yourdomain.com' />",
    "<meta property='og:description' content='Your app description here.' />",
    "<meta name='description' content='Your app description here.' />",
  ],
}
```

Make sure to customize the content to accurately describe your application.

## 2. Adding robots.txt

### Issue

The application may lack a valid `robots.txt` file, which search engines use to understand which pages to crawl.

### Solution

Create a `robots.txt` file in your `public` directory (alongside the favicon):

```txt title="public/robots.txt"
User-agent: *
Allow: /
Disallow: /admin/
Disallow: /api/
Disallow: /auth/
```

This configuration:

- Allows all search engine bots to crawl your site
- Blocks crawling of admin, API, and authentication routes

You may want to add a sitemap reference as well:

```txt title="public/robots.txt"
User-agent: *
Allow: /
Disallow: /admin/
Disallow: /api/
Disallow: /auth/

Sitemap: https://yourdomain.com/sitemap.xml
```

## 3. Adding Image Alt Attributes

### Issue

Some image elements may lack `alt` attributes, which are important for accessibility and SEO.

### Solution

Update image components to include descriptive alt attributes. For example, in the testimonials section:

```tsx title="src/landing-page/components/Testimonials.tsx"
<img
  src={testimonial.avatarSrc}
  alt={`Profile picture of ${testimonial.name}`}
  className="h-12 w-12 rounded-full"
/>
```

Review all images in your application and ensure they have meaningful alt text that describes the image content.

## Additional SEO Tips

### Add a Favicon

Ensure you have a proper favicon set up. Place your favicon files in the `public` directory:

- `favicon.ico` - for older browsers
- `favicon-32x32.png` - for modern browsers
- `apple-touch-icon.png` - for iOS devices

### Configure Open Graph Images

Add Open Graph images for better social media sharing:

```wasp title="main.wasp"
app OpenSaaS {
  head: [
    // ... other meta tags
    "<meta property='og:image' content='https://yourdomain.com/og-image.png' />",
    "<meta property='og:image:width' content='1200' />",
    "<meta property='og:image:height' content='630' />",
  ],
}
```

### Add Twitter Card Meta Tags

For better Twitter/X sharing:

```wasp title="main.wasp"
app OpenSaaS {
  head: [
    // ... other meta tags
    "<meta name='twitter:card' content='summary_large_image' />",
    "<meta name='twitter:title' content='My Open SaaS App' />",
    "<meta name='twitter:description' content='Your app description here.' />",
    "<meta name='twitter:image' content='https://yourdomain.com/twitter-image.png' />",
  ],
}
```

## Limitations

Since Wasp creates a Single Page Application (SPA), there are some SEO limitations to be aware of:

- Meta tags defined in `main.wasp` are rendered only in the initial HTML
- Most bots (LinkedIn, Facebook, Twitter) only read the initial HTML, not JavaScript-modified DOM
- Libraries like `react-helmet-async` only change the head in the browser DOM, not in the HTML that bots receive

For advanced SEO requirements, consider:

- Using a pre-render service like [Prerender.io](https://prerender.io/)
- Setting up server-side rendering (SSR) when Wasp adds support for it

## Conclusion

By implementing these optimizations:

1. Proper meta descriptions
2. A valid `robots.txt` file
3. Descriptive alt attributes for images

You can achieve a 100% SEO score and make your OpenSaaS application more discoverable to search engines.
