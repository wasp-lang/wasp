Here's a skeleton of how our `main.wasp.ts` should look like after we're done:

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
    // Routes referenced here (like the login route) are registered
    // automatically, so they don't have to be listed in `spec`.
    onAuthFailedRedirectTo: route("LoginRoute", "/login", page(LoginPage)),
  },
  spec: [
    // Defining the rest of the routes, pages, and other elements
    // ...
  ],
})
```
