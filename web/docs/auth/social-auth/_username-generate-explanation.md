Wasp exposes two functions that can help you generate usernames. Import them from `@wasp/core/auth.js`:

- `generateAvailableUsername` takes an array of strings and an optional separator and generates a string ending with a random number that is not yet in the database. For example, the above could produce something like "Jim.Smith.3984" for a Github user Jim Smith.
- `generateAvailableDictionaryUsername` generates a random dictionary phrase that is not yet in the database. For example, `nice-blue-horse-27160`.
