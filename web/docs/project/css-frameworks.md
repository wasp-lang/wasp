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
    import { wasp } from 'wasp/client/vite'
    // highlight-next-line
    import tailwindcss from '@tailwindcss/vite'
    import { defineConfig } from 'vite'

    export default defineConfig({
      plugins: [
        // highlight-next-line
        wasp(),
        tailwindcss()
      ],
      server: {
        open: true,
      },
    })
    ```

1. Import Tailwind into your CSS file. For example, in a new project you might import Tailwind into `Main.css`.

    ```css title="src/Main.css" {1}
    @import "tailwindcss";

    /* ... */
    ```

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

```css title="src/Main.css" {1,3-4}
@import "tailwindcss";

@plugin "@tailwindcss/forms";
@plugin "@tailwindcss/typography";

/* ... */
```

### Troubleshooting

#### Tailwind classes are not being applied

If your Tailwind classes aren't working, make sure the `wasp()` plugin is properly configured in your `vite.config.ts` file and that it's the **first plugin** in the plugins array.

<Tabs>
<TabItem value="check" label="Check your config">

```ts title="vite.config.ts"
import { wasp } from 'wasp/client/vite'
import tailwindcss from '@tailwindcss/vite'
import { defineConfig } from 'vite'

export default defineConfig({
  plugins: [
    wasp(),
    tailwindcss()
  ],
  server: {
    open: true,
  },
})
```

</TabItem>
<TabItem value="common" label="Common issues">

1. **`wasp()` plugin is not imported** - Make sure you have `import { wasp } from 'wasp/client/vite'` at the top of your config
2. **`wasp()` is not the first plugin** - The plugin must be listed before other plugins like `tailwindcss()`
3. **Tailwind import is missing** - Make sure your CSS file has `@import "tailwindcss";` (without `source(".")`)

</TabItem>
</Tabs>
