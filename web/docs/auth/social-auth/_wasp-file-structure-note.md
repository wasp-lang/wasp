Here's a skeleton of how our `main.wasp.ts` should look like after we're done:

```ts title="main.wasp.ts"
import { app, page, route } from "@wasp.sh/spec"
import { LoginPage } from "./src/pages/auth" with { type: "ref" }

// Configuring the social authentication
export default app({
  name: "myApp",
  wasp: { version: "{latestWaspVersion}" },
  title: "My App",
  auth: {
    // ...
  },
  decls: [
    // Defining routes and pages
    route("LoginRoute", "/login", page(LoginPage)),
  ],
})
```
