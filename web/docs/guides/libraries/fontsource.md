---
last_update:
  date: 2024-03-11
title: Fontsource
comments: true
---

# Fontsource

This guide shows you how to use [Fontsource](https://fontsource.org/) with Wasp to add custom fonts to your application. This also solves the "The request url \* is outside of Vite serving allow list" issue.

## Prerequisites

Make sure you have a Wasp project set up. If you haven't, follow the [Getting Started](../../introduction/quick-start.md) guide first.

## Setting up Fontsource

### 1. Install the font packages

Install the fonts you want to use from Fontsource. For example, to install Roboto and Bebas Neue:

```bash
npm install @fontsource/roboto @fontsource/bebas-neue
```

### 2. Configure Vite to allow serving font files

Create or update your `vite.config.ts` file in your project root:

```ts title="vite.config.ts"
import { resolveProjectPath } from "wasp/dev";
import { defineConfig, searchForWorkspaceRoot } from "vite";

export default defineConfig({
  server: {
    open: true,
    fs: {
      allow: [
        // Keeping the original behaviour how Vite searches for the workspace root
        // https://vitejs.dev/config/server-options#server-fs-allow
        searchForWorkspaceRoot(process.cwd()),
        // Allow serving files from the node_modules/@fontsource directory
        // (using Wasp's helper to resolve project path)
        resolveProjectPath("./node_modules/@fontsource"),
      ],
    },
  },
});
```

### 3. Import the fonts in your root component

Set up a root component in your `main.wasp` file:

```wasp title="main.wasp"
app MyApp {
  wasp: {
    version: "^0.15.0"
  },
  title: "My App",
  client: {
    rootComponent: import { App } from "@src/App.jsx"
  }
}
```

Then import the font CSS files in your root component:

```jsx title="src/App.jsx"
import "@fontsource/roboto/300.css";
import "@fontsource/roboto/400.css";
import "@fontsource/roboto/500.css";
import "@fontsource/roboto/700.css";
import "@fontsource/bebas-neue/400.css";

export const App = ({ children }) => {
  return <>{children}</>;
};
```

### 4. Use the fonts in your CSS

Now you can use the fonts in your CSS:

```css title="src/Main.css"
body {
  font-family: "Roboto", sans-serif;
}

h1,
h2,
h3 {
  font-family: "Bebas Neue", cursive;
}
```

That's it! Your fonts should now be working in your Wasp application.
