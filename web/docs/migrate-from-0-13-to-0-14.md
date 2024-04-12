---
title: Migration from 0.13.X to 0.14.X
---

:::note Are you on 0.11.X or earlier?

This guide only covers the migration from **0.13.X to 0.14.X**. If you are migrating from 0.11.X or earlier, please read the [migration guide from 0.11.X to 0.12.X](./migrate-from-0-11-to-0-12.md) first.

:::

## What's new in 0.14.0?

### Better auth user API

We introduced a much simpler API for accessing user auth fields like `username`, `email` or `isEmailVerified` on the `user` object. You don't need to use helper functions every time you want to access the user's `username` or extra steps to get proper typing.

## How to migrate?

### Migrate how you access user auth fields

We had to make a couple of breaking changes to reach the new simpler API. 

Follow the steps below to migrate:

1. **Replace the `getUsername` helper** with `user.identities.username.id`

    If you didn't use the `getUsername` helper in your code, you can skip this step.

    This helper changed and it no longer works with the `user` you receive as a prop on a page or through the `context`. You'll need to replace it with `user.identities.username.id`.

    <Tabs>
    <TabItem value="before" label="Before">

    ```tsx title="src/MainPage.tsx"
    import { getUsername, AuthUser } from 'wasp/auth'

    const MainPage = ({ user }: { user: AuthUser }) => {
        const username = getUsername(user)
        // ...
    }
    ```

    ```ts title=src/tasks.ts
    import { getUsername } from 'wasp/auth'

    export const createTask: CreateTask<...>  = async (args, context) => {
        const username = getUsername(context.user)
        // ...
    }
    ```

    </TabItem>
    <TabItem value="after" label="After">

    ```tsx title="src/MainPage.tsx"
    import { AuthUser } from 'wasp/auth'

    const MainPage = ({ user }: { user: AuthUser }) => {
        const username = user.identities.username?.id
        // ...
    }
    ```

    ```ts title=src/tasks.ts
    export const createTask: CreateTask<...>  = async (args, context) => {
        const username = context.user.identities.username?.id
        // ...
    }
    ```
    </TabItem>
    </Tabs>

1. **Replace the `getEmail` helper** with `user.identities.email.id`

    If you didn't use the `getEmail` helper in your code, you can skip this step.

    This helper changed and it no longer works with the `user` you receive as a prop on a page or through the `context`. You'll need to replace it with `user.identities.email.id`.

    <Tabs>
    <TabItem value="before" label="Before">

    ```tsx title="src/MainPage.tsx"
    import { getEmail, AuthUser } from 'wasp/auth'

    const MainPage = ({ user }: { user: AuthUser }) => {
        const email = getEmail(user)
        // ...
    }
    ```

    ```ts title=src/tasks.ts
    import { getEmail } from 'wasp/auth'

    export const createTask: CreateTask<...>  = async (args, context) => {
        const email = getEmail(context.user)
        // ...
    }
    ```

    </TabItem>
    <TabItem value="after" label="After">

    ```tsx title="src/MainPage.tsx"
    import { AuthUser } from 'wasp/auth'

    const MainPage = ({ user }: { user: AuthUser }) => {
        const email = user.identities.email?.id
        // ...
    }
    ```

    ```ts title=src/tasks.ts
    export const createTask: CreateTask<...>  = async (args, context) => {
        const email = context.user.identities.email?.id
        // ...
    }
    ```
    </TabItem>
    </Tabs>

1. **Replace accessing `providerData`** with `user.identities.<provider>.<value>`

    If you didn't use any data from the `providerData` object, you can skip this step.

    Replace `<provider>` with the provider name (for example `username`, `email`, `google`, `github`, etc.) and `<value>` with the field you want to access (for example `isEmailVerified`).

    <Tabs>
    <TabItem value="before" label="Before">

    ```tsx title="src/MainPage.tsx"
    import { findUserIdentity, AuthUser } from 'wasp/auth'

    function getProviderData(user: AuthUser) {
        const emailIdentity = findUserIdentity(user, 'email')
        // We needed this before check for proper type support
        return emailIdentity && 'isEmailVerified' in emailIdentity.providerData
            ? emailIdentity.providerData
            : null
    }

    const MainPage = ({ user }: { user: AuthUser }) => {
        const providerData = getProviderData(user)
        const isEmailVerified = providerData ? providerData.isEmailVerified : null
        // ...
    }
    ```
    </TabItem>
    <TabItem value="after" label="After">

    ```tsx title="src/MainPage.tsx"
    import { AuthUser } from 'wasp/auth'

    const MainPage = ({ user }: { user: AuthUser }) => {
        // The email object is properly typed, so we can access `isEmailVerified` directly
        const isEmailVerified = user.identities.email?.isEmailVerified
        // ...
    }
    ```
    </TabItem>
    </Tabs>

1. **Use `getFirstProviderUserId` directly** on the user object

    If you didn't use `getFirstProviderUserId` in your code, you can skip this step.

    You should replace `getFirstProviderUserId(user)` with `user.getFirstProviderUserId()`.

    <Tabs>
    <TabItem value="before" label="Before">

    ```tsx title="src/MainPage.tsx"
    import { getFirstProviderUserId, AuthUser } from 'wasp/auth'

    const MainPage = ({ user }: { user: AuthUser }) => {
        const userId = getFirstProviderUserId(user)
        // ...
    }
    ```

    ```ts title=src/tasks.ts
    import { getFirstProviderUserId } from 'wasp/auth'

    export const createTask: CreateTask<...>  = async (args, context) => {
        const userId = getFirstProviderUserId(context.user)
        // ...
    }
    ```

    </TabItem>
    <TabItem value="after" label="After">

       ```tsx title="src/MainPage.tsx"
    import { AuthUser } from 'wasp/auth'

    const MainPage = ({ user }: { user: AuthUser }) => {
        const userId = user.getFirstProviderUserId()
        // ...
    }
    ```

    ```ts title=src/tasks.ts
    export const createTask: CreateTask<...>  = async (args, context) => {
        const userId = user.getFirstProviderUserId()
        // ...
    }
    ```
    </TabItem>
    </Tabs>

1. **Replace `findUserIdentity`** with checks on `user.identities.<provider>`

    If you didn't use `findUserIdentity` in your code, you can skip this step.

    Instead of using `findUserIdentity` to get the identity object, you can directly check if the identity exists on the `identities` object.

    <Tabs>
    <TabItem value="before" label="Before">

    ```tsx title="src/MainPage.tsx"
    import { findUserIdentity, AuthUser } from 'wasp/auth'

    const MainPage = ({ user }: { user: AuthUser }) => {
        const usernameIdentity = findUserIdentity(user, 'username')
        if (usernameIdentity) {
            // ...
        }
    }
    ```

    ```ts title=src/tasks.ts
    import { findUserIdentity } from 'wasp/auth'

    export const createTask: CreateTask<...>  = async (args, context) => {
        const usernameIdentity = findUserIdentity(context.user, 'username')
        if (usernameIdentity) {
            // ...
        }
    }
    ```

    </TabItem>
    <TabItem value="after" label="After">

    ```tsx title="src/MainPage.tsx"
    import { AuthUser } from 'wasp/auth'

    const MainPage = ({ user }: { user: AuthUser }) => {
        if (user.identities.username) {
            // ...
        }
    }
    ```

    ```ts title=src/tasks.ts
    export const createTask: CreateTask<...>  = async (args, context) => {
        if (context.user.identities.username) {
            // ...
        }
    }
    ```
    </TabItem>
    </Tabs>


That's it!

You should now be able to run your app with the new Wasp 0.14.0. We recommend reading through the updated [Accessing User Data](./auth/entities/entities.md) section to get a better understanding of the new API.