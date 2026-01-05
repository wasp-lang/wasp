---
title: CSS Frameworks
---

import useBaseUrl from '@docusaurus/useBaseUrl';

## Tailwind

Wasp works great with [Tailwind CSS](https://tailwindcss.com/), a utility-first CSS framework. You can use Tailwind CSS v4 and newer, with the [Vite installation method](https://tailwindcss.com/docs/installation/using-vite).

### Adding Tailwind to your Wasp project

1. Install Tailwind and its Vite plugin.

    ```bash
    npm install tailwindcss
    npm install -D @tailwindcss/vite
    ```

1. Add the Tailwind CSS Vite plugin to your `vite.config.ts` file:

    ```ts title="vite.config.ts"
    // highlight-next-line
    import tailwindcss from '@tailwindcss/vite'
    import { defineConfig } from 'vite'

    export default defineConfig({
      server: {
        open: true,
      },
      plugins: [
        // highlight-next-line
        tailwindcss()
      ],
    })
    ```

1. Import Tailwind into your CSS file. For example, in a new project you might import Tailwind into `Main.css`.

    ```css title="src/Main.css" {1}
    @import "tailwindcss" source(".");

    /* ... */
    ```

    If your main CSS file is in a different location, adjust the path accordingly so that it points to the `src` folder.

1. Start using Tailwind 🥳

    ```tsx title="src/MainPage.tsx" auto-js
    // ...

    <h1 className="text-3xl font-bold underline">
      Hello world!
    </h1>

    // ...
    ```

### Adding Tailwind Plugins

Wasp doesn't require any special configuration to use Tailwind plugins. You can follow each plugin's installation instructions as you normally would.

For example, to add the [Tailwind Forms](https://github.com/tailwindlabs/tailwindcss-forms) and [Tailwind Typography](https://github.com/tailwindlabs/tailwindcss-typography) plugins, we can check the installation instructions on their respective documentation pages and follow them as usual:

```shell
npm install -D @tailwindcss/forms
npm install -D @tailwindcss/typography
```

```css title="src/Main.css" {3-4}
@import "tailwindcss" source(".");

@plugin "@tailwindcss/forms";
@plugin "@tailwindcss/typography";

/* ... */
```

### Troubleshooting

#### I followed the installation steps but Tailwind classes are not being applied

Due to how Wasp works internally, we need to tell Tailwind explicitly where to look for class names. While our guides walk you through this step, you might have missed it.

To fix this issue, find the CSS file with the `@import "tailwindcss"` directive and add `source(".")` to it.

If you used the default starter templates, the main CSS file will be located in the `src` dir, for example `src/Main.css` or `src/App.css`. If it is in a different location, adjust the path in `source()` accordingly, so that it points to your `src` folder.

<Tabs>
<TabItem value="before" label="Before">

```css title="src/Main.css" {1}
@import "tailwindcss";

/* ... */
```

</TabItem>
<TabItem value="after" label="After">

```css title="src/Main.css" {1}
@import "tailwindcss" source(".");

/* ... */
```

</TabItem>
</Tabs>
