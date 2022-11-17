---
title: Feature Release Announcement - Tailwind CSS support
authors: [shayneczyzewski]
image: /img/tailwind-2.png
tags: [webdev, wasp, feature, css]
---

import Link from '@docusaurus/Link';
import useBaseUrl from '@docusaurus/useBaseUrl';
import InBlogCta from './components/InBlogCta';
import WaspIntro from './_wasp-intro.md';
import ImgWithCaption from './components/ImgWithCaption'

<p align="center">
  <img alt="Full stack devs"
      src={useBaseUrl('img/tailwind-1.png')}
      width="400px"
  />
</p>

<!--truncate-->

<WaspIntro />
<InBlogCta />

There are backend devs who can do some frontend, and frontend devs who can do some backend. But the mythical full stack dev is exceedingly rare (or more likely, a lie). Even as someone who falls into the meme category above, we *all* still need to make websites that **look noice**. This is a place where CSS frameworks can help.

But which one should you use? According to our *extensive research*, a statistically-questionable-but-you’re-still-significant-to-us 11 people on Twitter wanted us to add better support for [Tailwind](https://tailwindcss.com/). Which was lucky for us, since we already added it before asking them. 😅

<p align="center">
  <img alt="Twitter voting"
      src={useBaseUrl('img/tailwind-2.png')}
      width="400px"
  />
</p>

Ok, it wasn’t a huge stretch for us to do so preemptively. Tailwind is one of the most heavily used CSS frameworks out there today and seems to keep growing in popularity. So how do you integrate it into your Wasp apps? Like many things in Wasp, it’s really easy- just drop in two config files into the root of your project and you can then start using it! Here are the defaults:

```jsx title="./tailwind.config.js"
/** @type {import('tailwindcss').Config} */
module.exports = {
  content: [
    "./src/**/*.{js,jsx,ts,tsx}",
  ],
  theme: {
    extend: {},
  },
  plugins: [],
}
```

```jsx title="./postcss.config.js"
module.exports = {
  plugins: {
    tailwindcss: {},
    autoprefixer: {},
  },
}
```

When these two files are present, Wasp will make sure all the required NPM dependencies get added, that [PostCSS](https://postcss.org/) plays nicely with Tailwind directives in CSS files, and that your JavaScript files are properly processed so you can use all the CSS selectors you want (provided you are properly equipped :D).

<p align="center">
  <img alt="Best monitor"
      src={useBaseUrl('img/tailwind-3.png')}
      width="500px"
  />
</p>

With that in place, you can add the Tailwind directives to your CSS files like so:

```css title="./src/client/Main.css"
@tailwind base;
@tailwind components;
@tailwind utilities;

/* rest of content below */
```

And then start using Tailwind classes in your components:

```jsx
<h1 className="text-3xl font-bold underline">
  Hello world!
</h1>
```

As usual, Wasp will still automatically reload your code and refresh the browser on any changes. 🥳

Lastly, here is a small example that shows how to add a few Tailwind plugins for the adventurous ([wasp file](https://github.com/wasp-lang/wasp/blob/main/waspc/examples/todoApp/todoApp.wasp#L8-L9) and [Tailwind config](https://github.com/wasp-lang/wasp/blob/main/waspc/examples/todoApp/tailwind.config.js#L10-L11)), and [here](/docs/integrations/css-frameworks) are the docs for more details. We can’t wait to see what you make!
