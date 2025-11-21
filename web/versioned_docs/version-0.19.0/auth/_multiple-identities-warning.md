:::caution Using multiple auth identities for a single user

Wasp currently doesn't support multiple auth identities for a single user. This means, for example, that a user can't have both an email-based auth identity and a Google-based auth identity. This is something we will add in the future with the introduction of the [account merging feature](https://github.com/wasp-lang/wasp/issues/954).

Account merging means that multiple auth identities can be merged into a single user account. For example, a user's email and Google identity can be merged into a single user account. Then the user can log in with either their email or Google account and they will be logged into the same account.
:::
