---
title: CSS Frameworks
---

import useBaseUrl from '@docusaurus/useBaseUrl';

## Tailwind

To enable support for Tailwind in your project, you need to add two config files — [`tailwind.config.cjs`](https://tailwindcss.com/docs/configuration#configuration-options) and `postcss.config.cjs` — to the root directory.

With these files present, Wasp installs the necessary dependencies and copies your configuration to the generated project. You can then use [Tailwind CSS directives](https://tailwindcss.com/docs/functions-and-directives#directives) in your CSS and Tailwind classes on your React components.

```bash title="tree ."
.
├── main.wasp
├── package.json
├── src
│   ├── Main.css
│   ├── MainPage.jsx
│   ├── vite-env.d.ts
│   └── waspLogo.png
├── public
├── tsconfig.json
├── vite.config.ts
# highlight-start
├── postcss.config.cjs
└── tailwind.config.cjs
# highlight-end
```

:::tip Tailwind not working?
If you can not use Tailwind after adding the required config files, make sure to restart `wasp start`. This is sometimes needed to ensure that Wasp picks up the changes and enables Tailwind integration.
:::

### Enabling Tailwind Step-by-Step

:::caution
Make sure to use the `.cjs` extension for these config files, if you name them with a `.js` extension, Wasp will not detect them.
:::

1. Add `./tailwind.config.cjs`.

  ```js title="./tailwind.config.cjs"
  const { resolveProjectPath } = require('wasp/dev')

  /** @type {import('tailwindcss').Config} */
  module.exports = {
    content: [resolveProjectPath('./src/**/*.{js,jsx,ts,tsx}')],
    theme: {
      extend: {},
    },
    plugins: [],
  }
  ```

2. Add `./postcss.config.cjs`.

  ```js title="./postcss.config.cjs"
  module.exports = {
    plugins: {
      tailwindcss: {},
      autoprefixer: {},
    },
  }
  ```

3. Import Tailwind into your CSS file. For example, in a new project you might import Tailwind into `Main.css`.

  ```css title="./src/Main.css" {1-3}
  @tailwind base;
  @tailwind components;
  @tailwind utilities;

  /* ... */
  ```

4. Start using Tailwind 🥳
  
  ```jsx title="./src/MainPage.jsx"
  // ...

  <h1 className="text-3xl font-bold underline">
    Hello world!
  </h1>

  // ...
  ```

### Adding Tailwind Plugins

To add Tailwind plugins, install them as npm development [dependencies](../project/dependencies) and add them to the plugins list in your `tailwind.config.cjs` file:

```shell
npm install -D @tailwindcss/forms
npm install -D @tailwindcss/typography
```

and also

```js title="./tailwind.config.cjs" {5-6}
/** @type {import('tailwindcss').Config} */
module.exports = {
  // ...
  plugins: [
    require('@tailwindcss/forms'),
    require('@tailwindcss/typography'),
  ],
  // ...
}
```
