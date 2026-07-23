Here's a skeleton of what our `main.wasp.ts` should look like after we're done:

```ts title="main.wasp.ts"
import { app, page, route } from "@wasp.sh/spec"
import { LoginPage } from "./src/pages/auth" with { type: "ref" }

// Configuring the social authentication
export default app({
  name: "myApp",
  wasp: { version: "{latestWaspVersion}" },
  title: "My App",
  head: ["<link rel='icon' href='/favicon.ico' />"],
  auth: {
    // ...
    onAuthFailedRedirectTo: route("LoginRoute", "/login", page(LoginPage)),
  },
  spec: [
    // Defining the rest of the routes, pages, and other elements
    // ...
  ],
})
```
