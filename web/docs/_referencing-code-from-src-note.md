:::note

When `main.wasp.ts` needs to point to your code, it uses imports like this:

```ts
import { MainPage } from './src/MainPage' with { type: "ref" }
```

Notice the `with { type: "ref" }` part at the end of the import statement. This tells Wasp to treat the import as a reference to your app's code, without running the imported code. For more details and examples, see [reference imports](/docs/general/spec#reference-imports).

:::
