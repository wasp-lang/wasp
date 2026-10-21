---
sidebar_label: From 0.26 to 0.27
---

# Migration from 0.26 to 0.27

import InstallInstructions from './_install-instructions.md'

<InstallInstructions version="0.27" />

## What's new in 0.27?

<!-- Nothing here yet -->

## How to migrate?

### 1. Bump the Wasp version

Update the version field in your Wasp config to `^0.27.0`.

<Tabs sideBySide>
  <TabItem value="before" label="Before">
    ```ts title="main.wasp.ts"
    export default app({
      // highlight-next-line
      wasp: { version: "^0.26.0" },
      // ...
    });
    ```
  </TabItem>
  <TabItem value="after" label="After">
    ```ts title="main.wasp.ts"
    export default app({
      // highlight-next-line
      wasp: { version: "^0.27.0" },
      // ...
    });
    ```
  </TabItem>
</Tabs>

And run the following command to update the Wasp libraries in your project:

```bash
wasp install
```

### 2. Enjoy your updated Wasp app

That's it!
