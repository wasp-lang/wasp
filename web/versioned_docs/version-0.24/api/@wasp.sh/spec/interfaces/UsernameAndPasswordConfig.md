# Interface: UsernameAndPasswordConfig

Username and password auth configuration.

## Example

```ts
import { app } from "@wasp.sh/spec"
import { userSignupFields } from "./src/auth" with { type: "ref" }

export default app({
  // ...
  auth: {
    userEntity: "User",
    methods: {
      usernameAndPassword: {
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
