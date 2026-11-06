<!-- This is the user-facing intro of the API reference. For the developer-facing docs, see README.md. -->

# wasp

The `wasp` package is the Wasp SDK: the code you import from your app's source files to use Wasp's features, such as operations, auth, and the router.

```ts title="src/MainPage.tsx"
import { useAuth } from "wasp/client/auth";
import { getTasks, useQuery } from "wasp/client/operations";
```

Each of the modules below is a path you can import from.

## Getting the package

You don't need to install anything, and the package isn't published to npm. The Wasp CLI generates it specifically for your app, based on your Wasp Spec and Prisma schema, and sets it up as an npm workspace in `.wasp/out/sdk/wasp`.
