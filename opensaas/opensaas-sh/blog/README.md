# Open SaaS Docs and Blog

This is the docs and blog for the [OpenSaaS.sh](https://opensaas.sh/) website, [![Built with Starlight](https://astro.badg.es/v2/built-with-starlight/tiny.svg)](https://starlight.astro.build)


## 🚀 Project Structure

Inside of your Astro + Starlight project, you'll see the following folders and files:

```
.
├── public/
├── src/
│   ├── assets/
│   ├── components/
│   ├── content/
│   │   ├── docs/
│   │   │   ├── blog/
│   │   │   ├── guides/
│   │   │   └── ...
│   │   └── config.ts
│   └── env.d.ts
├── astro.config.mjs
├── package.json
└── tsconfig.json
```

Starlight looks for `.md` or `.mdx` files in the `src/content/docs/` directory. Each file is exposed as a route based on its file name.

Blog posts are in the `src/content/docs/blog/` directory. Use `.mdx` files for blog posts.

Images can be added to `src/assets/` and embedded in Markdown with a relative link.

Static assets, like favicons and banner images, can be placed in the `public/` directory.

We have a number of custom components in `src/components/` that you can use in your blog posts and docs.

##  Custom Components

Custom components in the `src/components/` that replace default Starlight components are imported into the `astro.config.mjs` file:

```js
components: {
  SiteTitle: './src/components/MyHeader.astro',
  ThemeSelect: './src/components/MyThemeSelect.astro',
  Head: './src/components/HeadWithOGImage.astro',
  PageTitle: './src/components/TitleWithBannerImage.astro',
},
```

Other components can be imported into your blog posts and docs using the `import` statement:

```mdx
---
title: "Open SaaS Tutorial"
date: 2024-12-10
//...
---
import VideoPlayer from '../../../components/VideoPlayer.astro';
```

### HeadWithOGImage

This component is used to generate the Open Graph (OG) meta tags for the social media preview images for each doc and blog post. 

It checks if a banner image exists in `./public/banner-images` with the same name as the blog post but with a `.webp` extension, e.g. if the blog post is `2024-12-10-open-saas-tutorial.mdx`, it checks for `./public/banner-images/2024-12-10-open-saas-tutorial.webp`. If it does, it uses that image. If it doesn't, it uses the default banner image.

Generally, the default banner image is used for docs, and blog posts use a custom banner image.

### TitleWithBannerImage

This component uses the same image as the `HeadWithOGImage` component to display a banner image above the title of the blog post.

You can use the `hideBannerImage` prop to hide the banner image on the page:

```mdx
---
title: "Open SaaS Tutorial"
date: 2024-12-10
hideBannerImage: true
---
```

Because the same image in `./public/banner-images` is used for social media preview images and the banner image on the doc/blog page, `hideBannerImage: true` will hide the banner image on the doc/blog page, but still use that image for the social media preview image.

### VideoPlayer

This component is a wrapper around the `video` element that adds some default styles.

You can pass three props to the component:

- `src` (required): the path to the video file
- `lgWidth` (optional): the width of the video player on large screens greater than 425px. If no prop is passed the default is `55%`.
- `smWidth` (optional): the width of the video player on small screens less than 425px. If no prop is passed the default is `100%`.

```mdx
---
title: "Open SaaS Tutorial"
date: 2024-12-10
//...
---
import VideoPlayer from '../../../components/VideoPlayer.astro';

<VideoPlayer src="/videos/open-saas-tutorial.mp4" lgWidth="75%" smWidth="80%" />
```

### MyHeader

This component is a wrapper around the `Header` component from the `@astrojs/starlight` package.

It repositions the docs and blog links to the left, and adds a logo and a link to the home page, https://opensaas.sh.


## Authoring Content

The docs and blog are written in Markdown or MDX with some additional metadata:

```mdx
title: We Made the Most Annoying Cookie Banners Ever
date: 2024-11-26
tags:
  - cookie consent
  - saas
  - sideproject
  - hackathon
subtitle: and it was totally worth it
hideBannerImage: true
authors: vince
```

Most posts are written in MDX, which allows you to use jsx components in your blog posts. It's recommended to use the MDX extension for your editor, such as this one for [VSCode](https://marketplace.cursorapi.com/items?itemName=unifiedjs.vscode-mdx).

### Blog Post Metadata
`authors` is required and will display the authors of the blog post. To configure a new author, add the proper metadata to `astro.config.mjs` under plugins > starlightBlog > authors:

```js
authors: {
  vince: {
    name: 'Vince',
    title: 'Dev Rel @ Wasp',
    picture: '/CRAIG_ROCK.png', // Put author images in the `public` directory.
    url: 'https://wasp.sh',
  },
},
```

`subtitle` is optional and will display a subtitle below the title of the blog post.

`hideBannerImage` is optional and will hide the banner image in `./public/banner-images` on the blog post page if you only want it to be displayed as the social media preview image (remember, the same image is used for both the social media preview image and the banner image on the blog post page).

### Images

Images to be used in guides and posts are stored in `./src/assets` and are referenced in the blog posts with a relative path.

Banner images used for social media preview images, as well as cover images for blog posts, are stored in `./public/banner-images` and must always use the `.webp` extension. If a banner image is not found, the default banner image is used. (Note: banner images for docs are used only for social media preview images, where for blog posts the are used as social media preview images and as cover images on the blog post page unless the `hideBannerImage` metadata is set to `true`.)

See the [HeadWithOGImage](#headwithogimage) and [TitleWithBannerImage](#titlewithbannerimage) sections for more information.

Always use astro's `Image` component to embed images in your blog posts and docs as Astro will automatically optimize the images for the web.

```mdx
import { Image } from 'astro:assets';
import myImage from '../../../assets/my-image.jpg';

<Image src={myImage} alt="My Image" />
```

## Video

Videos to be used in blog posts are stored in `./src/assets/` and are referenced in the blog posts with a relative path, just like images.

Always use the `VideoPlayer` component to embed videos in your blog posts. See the [VideoPlayer component](#videoplayer) section for more information.
