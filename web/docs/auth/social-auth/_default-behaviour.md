When a user **signs in for the first time**, Wasp will create a new user account and link it to the chosen auth provider account for future logins.

Also, if the `userEntity` has:
- a `username` field, Wasp will set it to a random username (e.g. `nice-blue-horse-14357`),
- a `password` field, Wasp will set it to a random string

This is a historical coupling between `auth` methods that will be removed over time.

<!-- This snippet is used in overview.md, google.md and github.md -->