By default, Wasp doesn't store any information it receives from the social login provider. It only stores the user's ID specific to the provider.

There are two mechanisms used for overriding the default behavior:

- `userSignupFields`
- `configFn`

Let's explore them in more detail.

<!-- This snippet is used in {google,github,keycloak}.md -->
