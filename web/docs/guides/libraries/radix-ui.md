---
last_update:
  date: 2024-03-08
title: Radix UI
comments: true
---

# Radix UI

This guide shows you how to integrate [Radix UI](https://www.radix-ui.com/) component library with your Wasp application.

## Prerequisites

Make sure you have a Wasp project set up. If you haven't, follow the [Getting Started](../../getting-started.md) guide first.

## Setting up Radix UI

### 1. Install Radix UI Themes

Install the Radix UI themes package:

```bash
npm install @radix-ui/themes
```

### 2. Create a Layout component

Create a layout component that wraps your app with the Radix Theme provider:

```jsx title="src/Layout.jsx"
import "@radix-ui/themes/styles.css";
import { Theme } from "@radix-ui/themes";

export function Layout({ children }) {
  return <Theme>{children}</Theme>;
}
```

### 3. Configure the root component in main.wasp

Set your Layout as the root component:

```wasp title="main.wasp"
app MyApp {
  wasp: {
    version: "^0.15.0"
  },
  title: "My App",
  client: {
    rootComponent: import { Layout } from "@src/Layout",
  }
}
```

### 4. Use Radix UI components

Now you can use Radix UI components anywhere in your application:

```jsx title="src/MainPage.jsx"
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

That's it! You can now use the full Radix UI component library in your Wasp application.

## Customizing the Theme

You can customize the theme by passing props to the `Theme` component:

```jsx title="src/Layout.jsx"
import "@radix-ui/themes/styles.css";
import { Theme } from "@radix-ui/themes";

export function Layout({ children }) {
  return (
    <Theme accentColor="crimson" grayColor="sand" radius="large" scaling="95%">
      {children}
    </Theme>
  );
}
```

See the [Radix UI Themes documentation](https://www.radix-ui.com/themes/docs/overview/getting-started) for more customization options.
