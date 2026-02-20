---
comments: true
last_checked_with_versions:
    Wasp: "0.21"
    Shadcn: 2026-02-16
---

# Shadcn

## Setting up Shadcn in a Wasp project

We'll be loosely following [the Vite instructions for Shadcn](https://ui.shadcn.com/docs/installation/vite) since Wasp is using Vite + React. Some of the steps don't apply, so we've adjusted them accordingly.

You won't be able to use the `@` alias setup since it's not currently supported by Wasp. Because of this you'll need to adjust some imports when we generate components, but it should be fairly straightforward to do.

### 1. Add Tailwind CSS

If you haven't added Tailwind CSS to your Wasp project yet, follow the instructions in the [Tailwind CSS documentation](../../project/css-frameworks.md#tailwind) first.

### 2. Temporarily set up the `@` alias

We need to temporarily setup the `@` alias to pass Shadcn's "Preflight checks". We'll remove it later.

Add the following lines to your `tsconfig.json`:

```json title="tsconfig.json"
{
  "compilerOptions": {
    // ...
    // highlight-start
    "baseUrl": ".",
    "paths": {
      "@/*": ["./src/*"]
    }
    // highlight-end
    // ...
  }
}
```

### 3. Setup Shadcn

Go into your project directory and run:

```bash
npx shadcn@latest init
```

Select the options like this:

```bash
✔ Preflight checks.
✔ Verifying framework. Found Vite.
✔ Validating Tailwind CSS.
✔ Validating import alias.
✔ Which style would you like to use? › New York
✔ Which color would you like to use as the base color? › Neutral
✔ Would you like to use CSS variables for theming? … yes
✔ Writing components.json.
✔ Checking registry.
✔ Updating tailwind.config.js
✔ Updating src/Main.css
✔ Installing dependencies.
✔ Created 1 file:
- src/lib/utils.ts
```

### 4. Remove the `@` alias

Remove the lines we added in the `tsconfig.json`:

```diff title="tsconfig.json"
 {
   "compilerOptions": {
     // ...
-    "baseUrl": ".",
-    "paths": {
-      "@/*": ["./src/*"]
-    }
   }
 }
```

### 5. Adjust the `components.json`

Adjust the `aliases` in `components.json` to be:

```json title="components.json"
{
  "$schema": "https://ui.shadcn.com/schema.json",
  // ...
  "aliases": {
     // highlight-start
     "components": "src/components",
     "utils": "src/lib/utils",
     "ui": "src/components/ui",
     "lib": "src/lib",
     "hooks": "src/hooks"
     // highlight-end
   },
 }
```

## Adding a component

In this example, we'll add the `Button` component.

### 1. Use the `shadcn` CLI to add the component

We'll add a button component with:

```bash
npx shadcn@latest add button
```

### 2. Adjust the `utils` import

You'll notice that you now have a brand new `button.tsx` file in `src/components/ui`. We need to fix some import issues:

```diff title="src/components/ui/button.tsx"
 import * as React from "react"
 import { Slot } from "@radix-ui/react-slot"
 import { cva, type VariantProps } from "class-variance-authority"

-import { cn } from "src/lib/utils"
+import { cn } from "../../lib/utils"
```

### 3. Use the `Button` component

That's it, now you are ready to use the `Button` component!

```tsx title="src/MainPage.tsx"
import "./Main.css";

import { Button } from "./components/ui/button";

export const MainPage = () => {
  return (
    <div className="container">
      <Button>This works</Button>
    </div>
  );
};
```
