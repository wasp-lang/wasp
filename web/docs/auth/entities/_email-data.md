```ts
const emailIdentity = user.identities.email

// Email address the the user used to sign up, e.g. "fluffyllama@app.com".
emailIdentity.id

// `true` if the user has verified their email address.
emailIdentity.isEmailVerified

// Datetime when the email verification email was sent.
emailIdentity.emailVerificationSentAt

// Datetime when the last password reset email was sent.
emailIdentity.passwordResetSentAt
```