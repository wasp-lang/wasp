---
title: CSS Frameworks
---

import useBaseUrl from '@docusaurus/useBaseUrl';

## Tailwind

Wasp works great with [Tailwind CSS](https://v3.tailwindcss.com/), a utility-first CSS framework. Currently, Wasp supports Tailwind CSS v3, but we are [working on supporting v4](https://github.com/wasp-lang/wasp/issues/2483) as well. You can use Tailwind CSS in your Wasp project by following the steps below.

### Adding Tailwind to your Wasp project

1. Install Tailwind as a development dependency.

```bash
npm install -D tailwindcss@3.2.7
```

2. Add `./tailwind.config.js`.

```js title="./tailwind.config.js"
import { resolveProjectPath } from "wasp/dev";

/** @type {import('tailwindcss').Config} */
export default {
  content: [resolveProjectPath('./src/**/*.{js,jsx,ts,tsx}')],
  theme: {
    extend: {},
  },
  plugins: [],
}
```

:::note The `resolveProjectPath` function

Because Wasp copies the configuration files to the generated project, you must wrap any paths in the `content` array with the `resolveProjectPath` function. This function resolves the path to the generated project, so that Tailwind can find your source files.

:::

3. Add `./postcss.config.js`.

```js title="./postcss.config.js"
export default {
  plugins: {
    tailwindcss: {},
    autoprefixer: {},
  },
}
```

4. Import Tailwind into your CSS file. For example, in a new project you might import Tailwind into `Main.css`.

```css title="./src/Main.css" {1-3}
@tailwind base;
@tailwind components;
@tailwind utilities;

/* ... */
```

5. Start using Tailwind 🥳

```jsx title="./src/MainPage.jsx"
// ...

<h1 className="text-3xl font-bold underline">
  Hello world!
</h1>

// ...
```

### Adding Tailwind Plugins

To add Tailwind plugins, install them as npm development [dependencies](../project/dependencies) and add them to the plugins list in your `tailwind.config.js` file:

```shell
npm install -D @tailwindcss/forms
npm install -D @tailwindcss/typography
```

and also
```js title="./tailwind.config.js" {5-6}
// ...
import TailwindTypography from "@tailwindcss/typography";
import TailwindForms from "@tailwindcss/forms";

/** @type {import('tailwindcss').Config} */
export default {
  // ...
  plugins: [
    TailwindTypography,
    TailwindForms,
  ],
  // ...
}
```
