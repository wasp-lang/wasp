Here's a skeleton of how our `main.wasp` should look like after we're done:

```wasp title="main.wasp"
// Configuring the social authentication
app myApp {
  auth: { ... }
}

// Defining entities
entity User { ... }

// Defining routes and pages
route LoginRoute { ... }
page LoginPage { ... }
```
