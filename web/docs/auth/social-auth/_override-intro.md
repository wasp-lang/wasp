By default, Wasp doesn't store any information it receives from the social login provider. It only stores the user's ID specific to the provider.

There are two mechanisms (functions) used for overriding the default behavior:

- `getUserFieldsFn`
- `configFn`

Let's explore them in more detail.

<!-- This snippet is used in google.md and github.md -->
