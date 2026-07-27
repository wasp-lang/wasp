# Interface: EmailAuthConfig

Email auth configuration.

See the [Email auth docs](https://wasp.sh/docs/auth/email) for the full
signup, verification, and password reset flows.

## Example

```ts
import { app } from "@wasp.sh/spec"
import { userSignupFields } from "./src/auth" with { type: "ref" }
import {
  getVerificationEmailContent,
  getPasswordResetEmailContent,
} from "./src/auth/email" with { type: "ref" }

export default app({
  // ...
  auth: {
    userEntity: "User",
    methods: {
      email: {
        userSignupFields,
        fromField: {
          name: "My App",
          email: "hello@itsme.com",
        },
        emailVerification: {
          clientRoute: "EmailVerificationRoute",
          getEmailContentFn: getVerificationEmailContent,
        },
        passwordReset: {
          clientRoute: "PasswordResetRoute",
          getEmailContentFn: getPasswordResetEmailContent,
        },
      },
    },
    onAuthFailedRedirectTo: "/login",
  },
})
```

## Extends

- `BaseAuthMethodConfig`

## Properties

### emailVerification

> **emailVerification**: [`EmailFlowConfig`](EmailFlowConfig.md)

An object that specifies the details of the e-mail verification process.

***

### fromField

> **fromField**: [`EmailFromField`](EmailFromField.md)

Sender identity used for verification and password reset emails.

***

### passwordReset

> **passwordReset**: [`EmailFlowConfig`](EmailFlowConfig.md)

An object that specifies the password reset process.

***

### userSignupFields?

> `optional` **userSignupFields?**: [`Reference`](../type-aliases/Reference.md)\<`AnyObject`\>

Object that defines extra fields to save on the user during signup
(e.g. `firstName`, `address`). Each field name must exist on the
configured [Auth.userEntity](Auth.md#userentity).

Each field function receives the data sent from the client and returns
the value Wasp saves to the database. For social auth, this data includes
provider-specific profile information. If the value is invalid, throw an
error. The `password` field is excluded from this object and handled by
Wasp's auth backend.

See [Signup Fields Customization](https://wasp.sh/docs/auth/overview#signup-fields-customization).

#### Example

```ts
import { defineUserSignupFields } from 'wasp/server/auth'

export const userSignupFields = defineUserSignupFields({
  address: (data) => {
    if (!data.address) {
      throw new Error('Address is required')
    }
    return data.address
  },
  phone: (data) => data.phone,
})
```

#### Inherited from

`BaseAuthMethodConfig.userSignupFields`
