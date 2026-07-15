# Interface: EmailFlowConfig

Configuration for an email-based auth flow (verification or password reset).

## Properties

### clientRoute

> **clientRoute**: `string`

Name of the route that handles the link sent in the email (e.g.
`"EmailVerificationRoute"` or `"PasswordResetRoute"`).

The route must be defined in [App.spec](App.md#spec) with the [route](../functions/route.md)
constructor.

This route should handle the process of taking a token from the URL and
sending it to the server to verify the e-mail address. You can use our
[`verifyEmail`
action](https://wasp.sh/docs/auth/email/create-your-own-ui#verifyemail) and
[`resetPassword`
action](https://wasp.sh/docs/auth/email/create-your-own-ui#resetpassword)
helpers for that.

***

### getEmailContentFn?

> `optional` **getEmailContentFn?**: [`Reference`](../type-aliases/Reference.md)\<`AnyFunction`\>

Function that returns the email content (subject, html, text) Wasp sends
for this flow. Verification email functions receive
`{ verificationLink }`, and password reset email functions receive
`{ passwordResetLink }`. If omitted, Wasp uses a built-in default
template.

In TypeScript, you can type the function with the
`GetVerificationEmailContentFn` or `GetPasswordResetEmailContentFn` type
from `wasp/server/auth`.

#### Example

```ts title="src/email.ts"
import { GetVerificationEmailContentFn } from "wasp/server/auth"

export const getVerificationEmailContent: GetVerificationEmailContentFn = ({
  verificationLink,
}) => ({
  subject: "Verify your email",
  text: `Click the link below to verify your email: ${verificationLink}`,
  html: `
        <p>Click the link below to verify your email</p>
        <a href="${verificationLink}">Verify email</a>
    `,
})
```
