When a user logs in using a social login provider, the backend receives some data about the user.
Wasp lets you access this data inside the `userSignupFields` getters.

For example, the User entity can include a `displayName` field which you can set based on the details received from the provider.

Wasp also lets you customize the configuration of the providers' settings using the `configFn` function.

Let's use this example to show both fields in action:

<!-- This snippet is used in {google,github,keycloak}.md -->
