---
title: 2. Apply a Patch
---

import { TutorialAction } from './TutorialAction';

## Add a Test File

Let's add a new file `src/testUtils.ts` to our Wasp app:

<TutorialAction id="add-test-file" action="APPLY_PATCH">

```ts title="src/testUtils.ts"
export function testHelper() {
  return "test helper function";
}
```

</TutorialAction>
