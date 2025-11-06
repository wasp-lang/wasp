---
title: 2. Apply a Patch
---

import { TutorialAction } from './TutorialAction';

## Add a Test File

This is a minimal test tutorial to verify the APPLY_PATCH action.

<TutorialAction id="add-test-file" action="APPLY_PATCH">

```ts title="src/testUtils.ts"
export function testHelper() {
  return "test helper function";
}
```

</TutorialAction>
