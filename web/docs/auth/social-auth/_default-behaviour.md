When a user **signs in for the first time**, Wasp creates a new user account and links it to the chosen auth provider account for future logins.

Also, if the `userEntity` has:

- A `username` field: Wasp sets it to a random username (e.g. `nice-blue-horse-14357`).
- A `password` field: Wasp sets it to a random string.

This is a historical coupling between `auth` methods we plan to remove in the future.

<!-- This snippet is used in overview.md, google.md and github.md -->
