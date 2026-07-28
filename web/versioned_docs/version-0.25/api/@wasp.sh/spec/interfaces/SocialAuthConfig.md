# Interface: SocialAuthConfig

Social auth provider configuration.

By default, Wasp stores only the user's provider-specific ID. Use
`userSignupFields` to save provider account details or custom signup state
on your [Auth.userEntity](Auth.md#userentity). Each provider has provider-specific
`userSignupFields` and `configFn` behavior, documented in the
[Social Auth docs](https://wasp.sh/docs/auth/social-auth/overview).

## Example

```ts
import { app } from "@wasp.sh/spec"
import { getConfig, userSignupFields } from "./src/auth/google" with { type: "ref" }

export default app({
  // ...
  auth: {
    userEntity: "User",
    methods: {
      google: {
        configFn: getConfig,
        userSignupFields,
      },
    },
    onAuthFailedRedirectTo: "/login",
  },
})
```

## Extends

- `BaseAuthMethodConfig`

## Properties

### configFn?

> `optional` **configFn?**: [`Reference`](../type-aliases/Reference.md)\<`AnyFunction`\>

Function that returns provider-specific OAuth options (scopes, prompt,
etc.). Use this to request extra scopes, such as requesting email/profile
data from providers that do not include it by default, or to customize the
OAuth flow.

#### Example

```ts
export function getConfig() {
  return {
    scopes: ["openid", "profile", "email"],
  }
}
```

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
