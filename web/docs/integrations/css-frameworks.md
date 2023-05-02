---
title: CSS Frameworks
---

import useBaseUrl from '@docusaurus/useBaseUrl';

# CSS Frameworks

## Tailwind

To enable support for Tailwind in your Wasp project, you simply need to add two config files (`tailwind.config.cjs` and `postcss.config.cjs`) to the root directory. When they are present, Wasp will add the necessary NPM dependencies and copy your config files into the generated project output. You can then start adding [Tailwind CSS directives](https://tailwindcss.com/docs/functions-and-directives#directives) to your CSS files and `className`s to your React components.

### New project tree overview
```bash title="tree ." {6,13-14}
.
├── main.wasp
├── src
│   ├── client
│   │   ├── tsconfig.json
│   │   ├── Main.css
│   │   ├── MainPage.js
│   │   └── waspLogo.png
│   ├── server
│   │   └── tsconfig.json
│   └── shared
│       └── tsconfig.json
├── postcss.config.cjs
└── tailwind.config.cjs
```

### Tailwind integration steps

:::caution
You need to name the config files with the `.cjs` extension since they are CommonJS modules. If you name them with `.js` extension, Wasp will not be able to find them and Tailwind integration will not work.
:::

#### 1) Add `./tailwind.config.cjs`
```js title="./tailwind.config.cjs"
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

#### 2) Add `./postcss.config.cjs`
```js title="./postcss.config.cjs"
module.exports = {
  plugins: {
    tailwindcss: {},
    autoprefixer: {},
  },
}
```

#### 3) Update `./src/client/Main.css`
```css title="./src/client//Main.css" {1-3}
@tailwind base;
@tailwind components;
@tailwind utilities;

/* rest of content below */
```

#### 4) Start using Tailwind 🥳
```js title="./src/client/MainPage.js"
import './Main.css'

<h1 className="text-3xl font-bold underline">
  Hello world!
</h1>
```

### Adding Tailwind plugins
To add Tailwind plugins, add them to your `tailwind.config.cjs` file and `main.wasp` files:

```js title="./tailwind.config.cjs" {10-11}
/** @type {import('tailwindcss').Config} */
module.exports = {
  content: [
    "./src/**/*.{js,jsx,ts,tsx}",
  ],
  theme: {
    extend: {},
  },
  plugins: [
    require('@tailwindcss/forms'),
    require('@tailwindcss/typography'),
  ],
}
```

```js title="main.wasp" {4-5}
app todoApp {
  title: "Example App",
  dependencies: [
    ("@tailwindcss/forms", "^0.5.3"),
    ("@tailwindcss/typography", "^0.5.7")
  ],
  // ...
}
```
