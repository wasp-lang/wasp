---
comments: true
last_checked_with_versions:
  Wasp: "0.21"
  "Radix Themes": 3
---

# Radix Themes

This guide shows you how to integrate the [Radix Themes](https://www.radix-ui.com/themes) component library into your Wasp application.

## Setting up Radix Themes

### 1. Install Radix Themes

Install the Radix Themes package:

```bash
npm install @radix-ui/themes
```

### 2. Create a Root Component if it doesn't exist

Due to how Radix works, we'll need to have a single component that wraps all the pages in our app. In Wasp, this is done through the `rootComponent` configuration:

```wasp title="main.wasp"
app MyApp {
  wasp: {
    version: "^0.21.0"
  },
  title: "My App",
  client: {
    // highlight-next-line
    rootComponent: import { Layout } from "@src/Layout",
  }
}
```

If you already have a root component in your Wasp app, open that file and skip to the next step. If you don't have one, create a new file with an empty component that will serve as the root:

```tsx auto-js title="src/Layout.tsx"
import type { ReactNode } from "react";

export function Layout({ children }: { children?: ReactNode }) {
  return children;
}
```

### 3. Add Radix Themes to your root component

In your root component, we'll wrap the `children` with Radix Theme's `Theme` component, and import their CSS stylesheet:

```tsx auto-js title="src/Layout.tsx"
import type { ReactNode } from "react";
// highlight-next-line
import "@radix-ui/themes/styles.css";
// highlight-next-line
import { Theme } from "@radix-ui/themes";

export function Layout({ children }: { children?: ReactNode }) {
  // highlight-next-line
  return <Theme>{children}</Theme>;
}
```


### 4. Use Radix Themes components

Now you can use Radix Themes components anywhere in your application:

```tsx auto-js title="src/MainPage.tsx"
import { Flex, Text, Button } from "@radix-ui/themes";

export const MainPage = () => {
  return (
    <Flex direction="column" gap="2">
      <Text>Hello from Radix Themes :)</Text>
      <Button>Let's go</Button>
    </Flex>
  );
};
```

That's it!

## Customizing the theme

You can customize the theme by passing props to the `Theme` component:

```tsx auto-js title="src/Layout.tsx"
import type { ReactNode } from "react";
import "@radix-ui/themes/styles.css";
import { Theme } from "@radix-ui/themes";

export function Layout({ children }: { children?: ReactNode }) {
  return (
    // highlight-next-line
    <Theme accentColor="crimson" grayColor="sand" radius="large" scaling="95%">
      {children}
    </Theme>
  );
}
```

See the [Radix Themes documentation](https://www.radix-ui.com/themes/docs/overview/getting-started) for more customization options.
