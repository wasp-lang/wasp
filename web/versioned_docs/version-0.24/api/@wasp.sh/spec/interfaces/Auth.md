# Interface: Auth

Authentication configuration and lifecycle hooks.

See the [Auth overview](https://wasp.sh/docs/auth/overview) for the
supported auth methods and how the `User` entity is connected to auth.
If hooks are async, Wasp awaits them. All hooks receive `prisma` and `req`
in their input. Hook return values are ignored except for
[AuthHooks.onBeforeOAuthRedirect](#onbeforeoauthredirect), which can change the redirect URL.

In TypeScript, you can type each hook implementation with its matching
type from `wasp/server/auth` (e.g. `OnBeforeSignupHook`). See
[Auth Hooks](https://wasp.sh/docs/auth/auth-hooks) for the full hook
inputs and examples.

## Example

```ts
import { app } from "@wasp.sh/spec"

export default app({
  // ...
  auth: {
    userEntity: "User",
    methods: {
      usernameAndPassword: {}, // use this or email, not both
      google: {},
      gitHub: {},
    },
    onAuthFailedRedirectTo: "/login",
  },
})
```

## Extends

- `AuthHooks`

## Fields

### methods

> **methods**: [`AuthMethods`](../type-aliases/AuthMethods.md)

Enabled authentication methods.

***

### onAfterEmailVerified?

> `optional` **onAfterEmailVerified?**: [`Reference`](../type-aliases/Reference.md)\<`AnyFunction`\>

Called once, after the user verifies their email. Receives `email` and `user`.

#### Inherited from

`AuthHooks.onAfterEmailVerified`

***

### onAuthFailedRedirectTo

> **onAuthFailedRedirectTo**: `string`

Route that Wasp redirects unauthenticated users to when they try to
access a page that has `authRequired: true`.

See [Adding Auth to the Project](https://wasp.sh/docs/tutorial/auth#adding-auth-to-the-project)
for an example.

***

### onAuthSucceededRedirectTo?

> `optional` **onAuthSucceededRedirectTo?**: `string`

Route that Wasp redirects users to after a successful login or signup.

Only takes effect when using Wasp's built-in Auth UI.

See [Auth UI](https://wasp.sh/docs/auth/ui).

#### Default

```ts
"/"
```

***

### userEntity

> **userEntity**: `string`

Name of the Prisma model that represents the application user connected to
your business logic.

The user entity needs to have an ID field that uniquely identifies each
user. It can be of any name and type, but it needs to be marked with `@id`:

```prisma title="schema.prisma"
model User {
  id Int @id @default(autoincrement())
}
```

You can add any other fields you want to the user entity. Make sure to also
define them in the `userSignupFields` field if they need to be set during
the sign-up process.

See [Accessing User Data](https://wasp.sh/docs/auth/entities) for how the
user entity connects to the rest of the auth system.

## Hooks

### onAfterLogin?

> `optional` **onAfterLogin?**: [`Reference`](../type-aliases/Reference.md)\<`AnyFunction`\>

Called after a successful login. Receives `providerId`, `user`, and, for
social auth, `oauth` fields including tokens and the unique OAuth request
ID.

#### Inherited from

`AuthHooks.onAfterLogin`

***

### onAfterSignup?

> `optional` **onAfterSignup?**: [`Reference`](../type-aliases/Reference.md)\<`AnyFunction`\>

Called after the user is created. Receives `providerId`, the created
`user`, and, for social auth, `oauth` fields including tokens and the
unique OAuth request ID.

#### Inherited from

`AuthHooks.onAfterSignup`

***

### onBeforeLogin?

> `optional` **onBeforeLogin?**: [`Reference`](../type-aliases/Reference.md)\<`AnyFunction`\>

Called before the user is logged in. Receives `providerId` and `user`.
Throw from this hook to reject a login based on custom criteria.

#### Inherited from

`AuthHooks.onBeforeLogin`

***

### onBeforeOAuthRedirect?

> `optional` **onBeforeOAuthRedirect?**: [`Reference`](../type-aliases/Reference.md)\<`AnyFunction`\>

Called before redirecting the user to the OAuth provider. Receives the
generated `url` and `oauth.uniqueRequestId`. Return `{ url }` to override
the redirect URL.

#### Inherited from

`AuthHooks.onBeforeOAuthRedirect`

***

### onBeforeSignup?

> `optional` **onBeforeSignup?**: [`Reference`](../type-aliases/Reference.md)\<`AnyFunction`\>

Called before the user is created. Receives `providerId` plus the common
hook input. Throw from this hook to reject a signup based on custom
criteria.

#### Inherited from

`AuthHooks.onBeforeSignup`
