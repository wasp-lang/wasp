---
title: Accessing User Data
---

import ImgWithCaption from '@site/blog/components/ImgWithCaption'
import { Internal } from '@site/src/components/Tag'
import MultipleIdentitiesWarning from '../\_multiple-identities-warning.md';
import UsernameData from './\_username-data.md';
import EmailData from './\_email-data.md';
import GoogleData from './\_google-data.md';
import GithubData from './\_github-data.md';
import KeycloakData from './\_keycloak-data.md';
import DiscordData from './\_discord-data.md';

First, we'll check out the most practical info: **how to access the user's data in your app**.

Then, we'll dive into the details of the **auth entities** that Wasp creates behind the scenes to store the user's data. For auth each method, Wasp needs to store different information about the user. For example, username for [Username & password](./username-and-pass) auth, email verification status for [Email](./email) auth, and so on.

We'll also show you how you can use these entities to create a custom signup action.

## Accessing the Auth Fields

When you receive the `user` object [on the client or the server](../overview.md#accessing-the-logged-in-user), it will contain all the user fields you defined in the `User` entity in the `schema.prisma` file. In addition to that, it will also contain all the auth-related fields that Wasp stores. This includes things like the `username` or the email verification status. In Wasp, this data is called the `AuthUser` object.

### `AuthUser` Object Fields

All the `User` fields you defined will be present at the top level of the `AuthUser` object. The auth-related fields will be on the `identities` object. For each auth method you enable, there will be a separate data object in the `identities` object.

The `AuthUser` object will change depending on which auth method you have enabled in the Wasp file. For example, if you enabled the email auth and Google auth, it would look something like this:

<Tabs>
<TabItem value="google" label="User Signed Up with Google">

If the user has only the Google identity, the `AuthUser` object will look like this:

```ts
const user = {
  // User data
  id: 'cluqs9qyh00007cn73apj4hp7',
  address: 'Some address',

  // Auth methods specific data
  identities: {
    email: null,
    google: {
      id: '1117XXXX1301972049448',
    },
  },
}
```

</TabItem>

<TabItem value="email" label="User Signed Up with Email">

If the user has only the email identity, the `AuthUser` object will look like this:

```ts
const user = {
  // User data
  id: 'cluqsex9500017cn7i2hwsg17',
  address: 'Some address',

  // Auth methods specific data
  identities: {
    email: {
      id: 'user@app.com',
      isEmailVerified: true,
      emailVerificationSentAt: '2024-04-08T10:06:02.204Z',
      passwordResetSentAt: null,
    },
    google: null,
  },
}
```

</TabItem>
</Tabs>

In the examples above, you can see the `identities` object contains the `email` and `google` objects. The `email` object contains the email-related data and the `google` object contains the Google-related data.

:::info Make sure to check if the data exists

Before accessing some auth method's data, you'll need to check if that data exists for the user and then access it:

```ts
if (user.identities.google !== null) {
  const userId = user.identities.google.id
  // ...
}
```

You need to do this because if a user didn't sign up with some auth method, the data for that auth method will be `null`.
:::

Let's look at the data for each of the available auth methods:

- [Username & password](../username-and-pass.md) data

  <UsernameData />

- [Email](../email.md) data

  <EmailData />

- [Google](../social-auth/google.md) data

  <GoogleData />

- [GitHub](../social-auth/github.md) data

  <GithubData />

- [Keycloak](../social-auth/keycloak.md) data

  <KeycloakData />

- [Discord](../social-auth/discord.md) data

  <DiscordData />

If you support multiple auth methods, you'll need to find which identity exists for the user and then access its data:

```ts
if (user.identities.email !== null) {
  const email = user.identities.email.id
  // ...
} else if (user.identities.google !== null) {
  const googleId = user.identities.google.id
  // ...
}
```

### `getFirstProviderUserId` Helper

The `getFirstProviderUserId` method returns the first user ID that it finds for the user. For example if the user has signed up with email, it will return the email. If the user has signed up with Google, it will return the Google ID.

This can be useful if you support multiple authentication methods and you need _any_ ID that identifies the user in your app.

<Tabs groupId="js-ts">
<TabItem value="js" label="JavaScript">

```jsx title="src/MainPage.jsx"
const MainPage = ({ user }) => {
  const userId = user.getFirstProviderUserId()
  // ...
}
```

```js title=src/tasks.js
export const createTask = async (args, context) => {
  const userId = context.user.getFirstProviderUserId()
  // ...
}
```

</TabItem>
<TabItem value="ts" label="TypeScript">

```tsx title="src/MainPage.tsx"
import { type AuthUser } from 'wasp/auth'

const MainPage = ({ user }: { user: AuthUser }) => {
  const userId = user.getFirstProviderUserId()
  // ...
}
```

```ts title=src/tasks.ts
export const createTask: CreateTask<...>  = async (args, context) => {
  const userId = context.user.getFirstProviderUserId()
  // ...
}
```

</TabItem>
</Tabs>

<small>

\* Multiple identities per user will be possible in the future and then the `getFirstProviderUserId` method will return the ID of the first identity that it finds without any guarantees about which one it will be.

</small>

## Including the User with Other Entities

Sometimes, you might want to include the user's data when fetching other entities. For example, you might want to include the user's data with the tasks they have created.

We'll mention the `auth` and the `identities` relations which we will explain in more detail later in the [Entities Explained](#entities-explained) section.

:::caution Be careful about sensitive data

You'll need to include the `auth` and the `identities` relations to get the full auth data about the user. However, you should keep in mind that the `providerData` field in the `identities` can contain sensitive data like the user's hashed password (in case of email or username auth), so you will likely want to exclude it if you are returning those values to the client.

:::

You can include the full user's data with other entities using the `include` option in the Prisma queries:

<Tabs groupId="js-ts">
<TabItem value="js" label="JavaScript">

```js title="src/tasks.js"
export const getAllTasks = async (args, context) => {
  return context.entities.Task.findMany({
    orderBy: { id: 'desc' },
    select: {
      id: true,
      title: true,
      // highlight-next-line
      user: {
        include: {
          // highlight-next-line
          auth: {
            include: {
              // highlight-next-line
              identities: {
                // Including only the `providerName` and `providerUserId` fields
                select: {
                  providerName: true,
                  providerUserId: true,
                },
              },
            },
          },
        },
      },
    },
  })
}
```

</TabItem>
<TabItem value="ts" label="TypeScript">

```ts title="src/tasks.ts"
export const getAllTasks = (async (args, context) => {
  return context.entities.Task.findMany({
    orderBy: { id: 'desc' },
    select: {
      id: true,
      title: true,
      // highlight-next-line
      user: {
        include: {
          // highlight-next-line
          auth: {
            include: {
              // highlight-next-line
              identities: {
                // Including only the `providerName` and `providerUserId` fields
                select: {
                  providerName: true,
                  providerUserId: true,
                },
              },
            },
          },
        },
      },
    },
  })
}) satisfies tasks.GetAllQuery<{}, {}>
```

</TabItem>
</Tabs>

If you have some **piece of the auth data that you want to access frequently** (for example the `username`), it's best to store it at the top level of the `User` entity.

For example, save the `username` or `email` as a property on the `User` and you'll be able to access it without including the `auth` and `identities` fields. We show an example in the [Defining Extra Fields on the User Entity](../overview.md#1-defining-extra-fields) section of the docs.

### Getting Auth Data from the User Object

When you have the `user` object with the `auth` and `identities` fields, it can be a bit tedious to obtain the auth data (like username or Google ID) from it:

<Tabs groupId="js-ts">
<TabItem value="js" label="JavaScript">

```jsx title="src/MainPage.jsx"
function MainPage() {
  // ...
  return (
    <div className="tasks">
      {tasks.map((task) => (
        <div key={task.id} className="task">
          {task.title} by {task.user.auth?.identities[0].providerUserId}
        </div>
      ))}
    </div>
  )
}
```

</TabItem>
<TabItem value="ts" label="TypeScript">

```tsx title="src/MainPage.tsx"
function MainPage() {
  // ...
  return (
    <div className="tasks">
      {tasks.map((task) => (
        <div key={task.id} className="task">
          {task.title} by {task.user.auth?.identities[0].providerUserId}
        </div>
      ))}
    </div>
  )
}
```

</TabItem>
</Tabs>

Wasp offers a few helper methods to access the user's auth data when you retrieve the `user` like this. They are `getUsername`, `getEmail` and `getFirstProviderUserId`. They can be used both on the client and the server.

#### `getUsername`

It accepts the `user` object and if the user signed up with the [Username & password](./username-and-pass) auth method, it returns the username or `null` otherwise. The `user` object needs to have the `auth` and the `identities` relations included.

<Tabs groupId="js-ts">
<TabItem value="js" label="JavaScript">

```jsx title="src/MainPage.jsx"
import { getUsername } from 'wasp/auth'

function MainPage() {
  // ...
  return (
    <div className="tasks">
      {tasks.map((task) => (
        <div key={task.id} className="task">
          {task.title} by {getUsername(task.user)}
        </div>
      ))}
    </div>
  )
}
```

</TabItem>
<TabItem value="ts" label="TypeScript">

```tsx title="src/MainPage.tsx"
import { getUsername } from 'wasp/auth'

function MainPage() {
  // ...
  return (
    <div className="tasks">
      {tasks.map((task) => (
        <div key={task.id} className="task">
          {task.title} by {getUsername(task.user)}
        </div>
      ))}
    </div>
  )
}
```

</TabItem>
</Tabs>

#### `getEmail`

It accepts the `user` object and if the user signed up with the [Email](./email) auth method, it returns the email or `null` otherwise. The `user` object needs to have the `auth` and the `identities` relations included.

<Tabs groupId="js-ts">
<TabItem value="js" label="JavaScript">

```jsx title="src/MainPage.jsx"
import { getEmail } from 'wasp/auth'

function MainPage() {
  // ...
  return (
    <div className="tasks">
      {tasks.map((task) => (
        <div key={task.id} className="task">
          {task.title} by {getEmail(task.user)}
        </div>
      ))}
    </div>
  )
}
```

</TabItem>
<TabItem value="ts" label="TypeScript">

```tsx title="src/MainPage.tsx"
import { getEmail } from 'wasp/auth'

function MainPage() {
  // ...
  return (
    <div className="tasks">
      {tasks.map((task) => (
        <div key={task.id} className="task">
          {task.title} by {getEmail(task.user)}
        </div>
      ))}
    </div>
  )
}
```

</TabItem>
</Tabs>

#### `getFirstProviderUserId`

It returns the first user ID that it finds for the user. For example if the user has signed up with email, it will return the email. If the user has signed up with Google, it will return the Google ID. The `user` object needs to have the `auth` and the `identities` relations included.

<Tabs groupId="js-ts">
<TabItem value="js" label="JavaScript">

```jsx title="src/MainPage.jsx"
import { getFirstProviderUserId } from 'wasp/auth'

function MainPage() {
  // ...
  return (
    <div className="tasks">
      {tasks.map((task) => (
        <div key={task.id} className="task">
          {task.title} by {getFirstProviderUserId(task.user)}
        </div>
      ))}
    </div>
  )
}
```

</TabItem>
<TabItem value="ts" label="TypeScript">

```tsx title="src/MainPage.tsx"
import { getFirstProviderUserId } from 'wasp/auth'

function MainPage() {
  // ...
  return (
    <div className="tasks">
      {tasks.map((task) => (
        <div key={task.id} className="task">
          {task.title} by {getFirstProviderUserId(task.user)}
        </div>
      ))}
    </div>
  )
}
```

</TabItem>
</Tabs>

## Entities Explained

To store user's auth information, Wasp does a few things behind the scenes. Wasp takes your `schema.prisma` file and combines it with additional entities to create the final `schema.prisma` file that is used in your app.

In this section, we will explain which entities are created and how they are connected.

### User Entity

When you want to add authentication to your app, you need to specify the `userEntity` field.

For example, you might set it to `User`:

```wasp title="main.wasp"
app myApp {
  wasp: {
    version: "^0.15.0"
  },
  title: "My App",
  auth: {
    // highlight-next-line
    userEntity: User,
    // ...
  },
}
```

And define the `User` in the `schema.prisma` file:

```prisma title="schema.prisma"
model User {
  id Int @id @default(autoincrement())
  // Any other fields you want to store about the user
}
```

The `User` entity is a "business logic user" which represents a user of your app.

You can use this entity to store any information about the user that you want to store. For example, you might want to store the user's name or address.

You can also use the user entity to define the relations between users and other entities in your app. For example, you might want to define a relation between a user and the tasks that they have created.

You **own** the user entity and you can modify it as you wish. You can add new fields to it, remove fields from it, or change the type of the fields. You can also add new relations to it or remove existing relations from it.

<ImgWithCaption alt="Auth Entities in a Wasp App" source="img/auth-entities/model.png" caption="Auth Entities in a Wasp App"/>

On the other hand, the `Auth`, `AuthIdentity` and `Session` entities are created behind the scenes and are used to store the user's login credentials. You as the developer don't need to care about this entity most of the time. Wasp **owns** these entities.

In the case you want to create a custom signup action, you will need to use the `Auth` and `AuthIdentity` entities directly.

### Example App Model

Let's imagine we created a simple tasks management app:

- The app has email and Google-based auth.
- Users can create tasks and see the tasks that they have created.

Let's look at how would that look in the database:

<ImgWithCaption alt="Example of Auth Entities" source="img/auth-entities/model-example.png" caption="Example of Auth Entities"/>

If we take a look at an example user in the database, we can see:

- The business logic user, `User` is connected to multiple `Task` entities.
  - In this example, "Example User" has two tasks.
- The `User` is connected to exactly one `Auth` entity.
- Each `Auth` entity can have multiple `AuthIdentity` entities.
  - In this example, the `Auth` entity has two `AuthIdentity` entities: one for the email-based auth and one for the Google-based auth.
- Each `Auth` entity can have multiple `Session` entities.
  - In this example, the `Auth` entity has one `Session` entity.

<MultipleIdentitiesWarning />

### `Auth` Entity <Internal />

Wasp's internal `Auth` entity is used to connect the business logic user, `User` with the user's login credentials.

```prisma
model Auth {
  id         String         @id @default(uuid())
  userId     Int?           @unique
  // Wasp injects this relation on the User entity as well
  user       User?          @relation(fields: [userId], references: [id], onDelete: Cascade)
  identities AuthIdentity[]
  sessions   Session[]
}
```

The `Auth` fields:

- `id` is a unique identifier of the `Auth` entity.
- `userId` is a foreign key to the `User` entity.
  - It is used to connect the `Auth` entity with the business logic user.
- `user` is a relation to the `User` entity.
  - This relation is injected on the `User` entity as well.
- `identities` is a relation to the `AuthIdentity` entity.
- `sessions` is a relation to the `Session` entity.

### `AuthIdentity` Entity <Internal />

The `AuthIdentity` entity is used to store the user's login credentials for various authentication methods.

```prisma
model AuthIdentity {
  providerName   String
  providerUserId String
  providerData   String @default("{}")
  authId         String
  auth           Auth   @relation(fields: [authId], references: [id], onDelete: Cascade)

  @@id([providerName, providerUserId])
}
```

The `AuthIdentity` fields:

- `providerName` is the name of the authentication provider.
  - For example, `email` or `google`.
- `providerUserId` is the user's ID in the authentication provider.
  - For example, the user's email or Google ID.
- `providerData` is a JSON string that contains additional data about the user from the authentication provider.
  - For example, for password based auth, this field contains the user's hashed password.
  - This field is a `String` and not a `Json` type because [Prisma doesn't support the `Json` type for SQLite](https://github.com/prisma/prisma/issues/3786).
- `authId` is a foreign key to the `Auth` entity.
  - It is used to connect the `AuthIdentity` entity with the `Auth` entity.
- `auth` is a relation to the `Auth` entity.

### `Session` Entity <Internal />

The `Session` entity is used to store the user's session information. It is used to keep the user logged in between page refreshes.

```prisma
model Session {
  id        String   @id @unique
  expiresAt DateTime
  userId    String
  auth      Auth     @relation(references: [id], fields: [userId], onDelete: Cascade)

  @@index([userId])
}
```

The `Session` fields:

- `id` is a unique identifier of the `Session` entity.
- `expiresAt` is the date when the session expires.
- `userId` is a foreign key to the `Auth` entity.
  - It is used to connect the `Session` entity with the `Auth` entity.
- `auth` is a relation to the `Auth` entity.

## Custom Signup Action

Let's take a look at how you can use the `Auth` and `AuthIdentity` entities to create custom login and signup actions. For example, you might want to create a custom signup action that creates a user in your app and also creates a user in a third-party service.

:::info Custom Signup Examples

In the [Email](./email#creating-a-custom-sign-up-action) section of the docs we give you an example for custom email signup and in the [Username & password](./username-and-pass#2-creating-your-custom-sign-up-action) section of the docs we give you an example for custom username & password signup.
:::

Below is a simplified version of a custom signup action which you probably wouldn't use in your app but it shows you how you can use the `Auth` and `AuthIdentity` entities to create a custom signup action.

<Tabs groupId="js-ts">
<TabItem value="js" label="JavaScript">

```wasp title="main.wasp"
// ...

action customSignup {
  fn: import { signup } from "@src/auth/signup.js",
  entities: [User]
}
```

```js title="src/auth/signup.js"
import {
  createProviderId,
  sanitizeAndSerializeProviderData,
  createUser,
} from 'wasp/server/auth'

export const signup = async (args, { entities: { User } }) => {
  try {
    // Provider ID is a combination of the provider name and the provider user ID
    // And it is used to uniquely identify the user in your app
    const providerId = createProviderId('username', args.username)
    // sanitizeAndSerializeProviderData hashes the password and returns a JSON string
    const providerData = await sanitizeAndSerializeProviderData({
      hashedPassword: args.password,
    })

    await createUser(
      providerId,
      providerData,
      // Any additional data you want to store on the User entity
      {}
    )

    // This is equivalent to:
    // await User.create({
    //   data: {
    //     auth: {
    //       create: {
    //         identities: {
    //             create: {
    //                 providerName: 'username',
    //                 providerUserId: args.username
    //                 providerData,
    //             },
    //         },
    //       }
    //     },
    //   }
    // })
  } catch (e) {
    return {
      success: false,
      message: e.message,
    }
  }

  // Your custom code after sign-up.
  // ...

  return {
    success: true,
    message: 'User created successfully',
  }
}
```

</TabItem>
<TabItem value="ts" label="TypeScript">

```wasp title="main.wasp"
// ...

action customSignup {
  fn: import { signup } from "@src/auth/signup.js",
  entities: [User]
}
```

```ts title="src/auth/signup.ts"
import {
  createProviderId,
  sanitizeAndSerializeProviderData,
  createUser,
} from 'wasp/server/auth'
import type { CustomSignup } from 'wasp/server/operations'

type CustomSignupInput = {
  username: string
  password: string
}
type CustomSignupOutput = {
  success: boolean
  message: string
}

export const signup: CustomSignup<
  CustomSignupInput,
  CustomSignupOutput
> = async (args, { entities: { User } }) => {
  try {
    // Provider ID is a combination of the provider name and the provider user ID
    // And it is used to uniquely identify the user in your app
    const providerId = createProviderId('username', args.username)
    // sanitizeAndSerializeProviderData hashes the password and returns a JSON string
    const providerData = await sanitizeAndSerializeProviderData<'username'>({
      hashedPassword: args.password,
    })

    await createUser(
      providerId,
      providerData,
      // Any additional data you want to store on the User entity
      {}
    )

    // This is equivalent to:
    // await User.create({
    //   data: {
    //     auth: {
    //       create: {
    //         identities: {
    //             create: {
    //                 providerName: 'username',
    //                 providerUserId: args.username
    //                 providerData,
    //             },
    //         },
    //       }
    //     },
    //   }
    // })
  } catch (e) {
    return {
      success: false,
      message: e.message,
    }
  }

  // Your custom code after sign-up.
  // ...

  return {
    success: true,
    message: 'User created successfully',
  }
}
```

</TabItem>
</Tabs>

You can use whichever method suits your needs better: either the `createUser` function or Prisma's `User.create` method. The `createUser` function is a bit more convenient to use because it hides some of the complexity. On the other hand, the `User.create` method gives you more control over the data that is stored in the `Auth` and `AuthIdentity` entities.
