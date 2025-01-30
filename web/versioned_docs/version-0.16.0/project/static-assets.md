---
title: Static Asset Handling
---

import { ShowForJs, ShowForTs } from '@site/src/components/TsJsHelpers'

## Importing an Asset as URL

Importing a static asset (e.g. an image) will return its URL. For example:

<Tabs groupId="js-ts">
<TabItem value="js" label="JavaScript">

```jsx title="src/App.jsx"
import imgUrl from './img.png'

function App() {
  return <img src={imgUrl} alt="img" />
}
```
</TabItem>
<TabItem value="ts" label="TypeScript">

```jsx title="src/App.tsx"
import imgUrl from './img.png'

function App() {
  return <img src={imgUrl} alt="img" />
}
```
</TabItem>
</Tabs>

For example, `imgUrl` will be `/img.png` during development, and become `/assets/img.2d8efhg.png` in the production build.

This is what you want to use most of the time, as it ensures that the asset file exists and is included in the bundle.

We are using Vite under the hood, read more about importing static assets in Vite's [docs](https://vitejs.dev/guide/assets.html#importing-asset-as-url).

## The `public` Directory

If you have assets that are:

- Never referenced in source code (e.g. robots.txt)
- Must retain the exact same file name (without hashing)
- ...or you simply don't want to have to import an asset first just to get its URL

Then you can place the asset in the `public` directory at the root of your project:

```
.
└── public
    ├── favicon.ico
    └── robots.txt
```

Assets in this directory will be served at root path `/` during development and copied to the root of the dist directory as-is.

For example, if you have a file `favicon.ico` in the `public` directory, and your app is hosted at `https://myapp.com`, it will be made available at `https://myapp.com/favicon.ico`.

:::info Usage in client code
Note that:

- You should always reference public assets using root absolute path
    - for example, `public/icon.png` should be referenced in source code as `/icon.png`.
- Assets in the `public` directory **cannot be imported** from <ShowForJs>JavaScript</ShowForJs><ShowForTs>TypeScript</ShowForTs>.
:::
