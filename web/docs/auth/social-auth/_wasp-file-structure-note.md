Structure of the `main.wasp` file we will end up with:

```wasp title="main.wasp"
// Configuring the social authentication
app myApp {
  auth: { ... }
}

// Defining entities
entity User { ... }
entity SocialLogin { ... }

// Defining routes and pages
route LoginRoute { ... }
page LoginPage { ... }
```