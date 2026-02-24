---
comments: true
last_checked_with_versions:
  Wasp: "0.21"
  Tailwind: 4
---

# Tailwind CSS

Wasp works great with [Tailwind CSS](https://tailwindcss.com/), a utility-first CSS framework. You can use Tailwind CSS by setting it up through their [Vite installation method](https://tailwindcss.com/docs/installation/using-vite), as with any other project.

## Adding Tailwind to your Wasp project

1. Install Tailwind and its Vite plugin.

    ```bash
    npm install tailwindcss
    npm install -D @tailwindcss/vite
    ```

1. Add the Tailwind CSS Vite plugin to your `vite.config.ts` file:

    ```ts title="vite.config.ts"
    import { wasp } from 'wasp/client/vite'
    // highlight-next-line
    import tailwindcss from '@tailwindcss/vite'
    import { defineConfig } from 'vite'

    export default defineConfig({
      plugins: [
        wasp(),
        // highlight-next-line
        tailwindcss()
      ],
      server: {
        open: true,
      },
    })
    ```

1. Import Tailwind into your base CSS file. For example, in a project created with `wasp new` you might import Tailwind into `Main.css`.

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

## Adding Tailwind Plugins

Wasp doesn't require any special configuration to use Tailwind plugins. You can follow each plugin's installation instructions as you normally would.

For example, to add the [Tailwind Forms](https://github.com/tailwindlabs/tailwindcss-forms) and [Tailwind Typography](https://github.com/tailwindlabs/tailwindcss-typography) plugins, we can check the installation instructions on their respective documentation pages and follow them as usual:

```shell
npm install -D @tailwindcss/forms
npm install -D @tailwindcss/typography
```

```css title="src/Main.css" {3-4}
@import "tailwindcss";

@plugin "@tailwindcss/forms";
@plugin "@tailwindcss/typography";

/* ... */
```
