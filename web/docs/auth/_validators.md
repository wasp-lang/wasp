We suggest using the built-in field validators for your authentication flow. You can import them from `wasp/server/auth`. These are the same validators that Wasp uses internally for the default authentication flow.

### Email Validators

- `ensureValidEmail(args)`

  Ensures that the provided email is valid.

- `ensurePasswordIsPresent(args)`

  Ensures that the password is present in the provided arguments.

- `ensureValidPassword(args)`

  Ensures that the provided password meets the required criteria.

### Username Validators

- `ensureValidUsername(args)`

  Ensures that the provided username is valid.
