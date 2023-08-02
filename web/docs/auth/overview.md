---
title: Using Auth
---

import { AuthMethodsGrid } from "@site/src/components/AuthMethodsGrid";
import { Required } from "@site/src/components/Required";

Auth is an essential piece of any serious application. Coincidentally, Wasp provides authentication and authorization support out of the box 🙃.

Enabling auth for your app is optional and can be done by configuring the `auth` field of the `app` declaration.  

<Tabs groupId="js-ts">
<TabItem value="js" label="JavaScript">

```wasp
app MyApp {
  title: "My app",
  //...
  auth: {
    userEntity: User,
    externalAuthEntity: SocialLogin,
    methods: {
      usernameAndPassword: {}, // use this or email, not both
      email: {}, // use this or usernameAndPassword, not both
      google: {},
      gitHub: {},
    },
    onAuthFailedRedirectTo: "/someRoute"
  }
}

//...
```
</TabItem>
<TabItem value="ts" label="TypeScript">

```wasp
app MyApp {
  title: "My app",
  //...
  auth: {
    userEntity: User,
    externalAuthEntity: SocialLogin,
    methods: {
      usernameAndPassword: {}, // use this or email, not both
      email: {}, // use this or usernameAndPassword, not both
      google: {},
      gitHub: {},
    },
    onAuthFailedRedirectTo: "/someRoute"
  }
}

//...
```
</TabItem>
</Tabs>

<small>

  Read more about the `auth` field options in the [Options Reference](#options-reference) section.

</small>

We will provide a quick overview of auth in Wasp and link to more detailed documentation for each auth method.


## Available auth methods

Wasp supports the following auth methods:

<AuthMethodsGrid />

Let's say we enabled the [Username & password](docs/auth/username-and-pass) authentication.

We get an auth backend with signup and login endpoints. We also get the `user` object in our [Operations](/docs/database/operations) and we can decide what to do based on whether the user is logged in or not. 

We would also get the [Auth UI](/docs/auth/ui) generated for us. We can set up our login and signup pages where our users can **create their account** and **login**. We can then protect certain pages by setting `authRequired: true` for them. This will make sure that only logged-in users can access them.

We will also have access to the `user` object in our frontend code, so we can show different UI to logged-in and logged-out users. For example, we can show the user's name in the header alongside a **logout button** or a login button if the user is not logged in.

## Protecting a page with `authRequired`

When you declare a `page` you can provide it with `authRequired` property.

If you set it to `true`, only authenticated users will be able to access this page. Unauthenticated users will be redirected to a route defined by `onAuthFailedRedirectTo` property within `app.auth`. 

<Tabs groupId="js-ts">
<TabItem value="js" label="JavaScript">

```wasp
page MainPage {
  component: import Main from "@client/pages/Main.jsx",
  authRequired: true
}
```
</TabItem>
<TabItem value="ts" label="TypeScript">

```wasp
page MainPage {
  component: import Main from "@client/pages/Main.tsx",
  authRequired: true
}
```
</TabItem>
</Tabs>

:::caution Requires auth method
`authRequired` can only be used if some auth method is enabled for the app.
:::

If `authRequired` is set to `true`, the React component of the page (specified by `component` property) will be provided `user` object as a prop. Read more about the `user` object in the [Accessing the logged-in user](#accessing-the-logged-in-user) section.

## Logout action

We provide an action for logging out the user. Here's how you can use it:

<Tabs groupId="js-ts">
<TabItem value="js" label="JavaScript">

```jsx title="client/components/LogoutButton.jsx"
import logout from '@wasp/auth/logout'

const LogoutButton = () => {
  return (
    <button onClick={logout}>Logout</button>
  )
}
```
</TabItem>
<TabItem value="ts" label="TypeScript">

```tsx title="client/components/LogoutButton.tsx"
import logout from '@wasp/auth/logout'

const LogoutButton = () => {
  return (
    <button onClick={logout}>Logout</button>
  )
}
```
</TabItem>
</Tabs>

## Accessing the logged-in user

You can get access to the `user` object both in the backend and on the frontend.

### On the client

#### Using the `user` prop

If `authRequired` is set to `true` for a `page` declaration, the React component of the page will be provided `user` object as a prop:

<Tabs groupId="js-ts">
<TabItem value="js" label="JavaScript">

```wasp title="main.wasp"
// ...

page AccountPage {
  component: import Account from "@client/pages/Account.jsx",
  authRequired: true
}
```

```jsx title="client/pages/Account.jsx"
import Button from './Button';
import logout from '@wasp/auth/logout';

const AccountPage = ({ user }) => {
  return (
    <div>
      <Button onClick={logout}>Logout</Button>
      {JSON.stringify(user, null, 2)}
    </div>
  );
};

export default AccountPage;
```
</TabItem>
<TabItem value="ts" label="TypeScript">

```wasp title="main.wasp"
// ...

page AccountPage {
  component: import Account from "@client/pages/Account.tsx",
  authRequired: true
}
```

```tsx title="client/pages/Account.tsx"
import type { User } from '@wasp/entities';
import Button from './Button';
import logout from '@wasp/auth/logout';

const AccountPage = ({ user }: { user: User }) => {
  return (
    <div>
      <Button onClick={logout}>Logout</Button>
      {JSON.stringify(user, null, 2)}
    </div>
  );
};

export default AccountPage;
```
</TabItem>
</Tabs>

#### Using the `useAuth` hook

Wasp provides a React hook you can use in the client components - `useAuth`.

This hook is a thin wrapper over Wasp's `useQuery` hook and returns data in the same format.

<Tabs groupId="js-ts">
<TabItem value="js" label="JavaScript">

```jsx title="src/client/pages/MainPage.jsx"
import useAuth from '@wasp/auth/useAuth'
import { Link } from 'react-router-dom'
import logout from '@wasp/auth/logout'
import Todo from '../Todo'

export function Main() {
  const { data: user } = useAuth()

  if (!user) {
    return (
      <span>
        Please <Link to='/login'>login</Link> or <Link to='/signup'>sign up</Link>.
      </span>
    )
  } else {
    return (
      <>
        <button onClick={logout}>Logout</button>
        <Todo />
      < />
    )
  }
}
```
</TabItem>
<TabItem value="ts" label="TypeScript">

```tsx title="src/client/pages/MainPage.tsx"
import useAuth from '@wasp/auth/useAuth'
import { Link } from 'react-router-dom'
import logout from '@wasp/auth/logout'
import Todo from '../Todo'

export function Main() {
  const { data: user } = useAuth()

  if (!user) {
    return (
      <span>
        Please <Link to='/login'>login</Link> or <Link to='/signup'>sign up</Link>.
      </span>
    )
  } else {
    return (
      <>
        <button onClick={logout}>Logout</button>
        <Todo />
      < />
    )
  }
}
```
</TabItem>
</Tabs>

### On the server

#### Using the `context.user`

When authentication is enabled, all [queries and actions](/docs/database/operations) will have access to the `user` through the `context` argument. `context.user` will contain all the fields from the user entity except for the password.

<Tabs groupId="js-ts">
<TabItem value="js" label="JavaScript">

```js title="src/server/actions.js"
import HttpError from '@wasp/core/HttpError.js'

export const createTask = async (task, context) => {
  if (!context.user) {
    throw new HttpError(403)
  }

  const Task = context.entities.Task
  return Task.create({
    data: {
      description: task.description,
      user: {
        connect: { id: context.user.id }
      }
    }
  })
}
```
</TabItem>
<TabItem value="ts" label="TypeScript">

```ts title="src/server/actions.ts"
import type { Task } from "@wasp/entities"
import type { CreateTask } from "@wasp/actions/types"
import HttpError from '@wasp/core/HttpError.js'

type CreateTaskPayload = Pick<Task, "description">

export const createTask: CreateTask<CreateTaskPayload, Task> = async (args, context) => {
  if (!context.user) {
    throw new HttpError(403)
  }

  const Task = context.entities.Task
  return Task.create({
    data: {
      description: args.description,
      user: {
        connect: { id: context.user.id }
      }
    }
  })
}
```
</TabItem>
</Tabs>

To implement access control, each operation is responsible for checking `context.user` and acting accordingly - e.g. if `context.user` is `undefined` and the operation is private then user should be denied access to it.

When using WebSockets, the `user` object is also available on the `socket.data`. Read more in the [WebSockets](/docs/advanced/web-sockets#websocketfn-function) section.

## User entity

### Password hashing

You don't need to worry about hashing the password yourself. Even when you are using Prisma's client directly and calling `create()` with a plain-text password, Wasp's middleware takes care of hashing it before storing it in the database.

For example, if you need to update a user's password, you can do it safely via Prisma client, e.g. within an action:

<Tabs groupId="js-ts">
<TabItem value="js" label="JavaScript">

```js title="src/server/actions.js"
export const updatePassword = async (args, context) => {
  return context.entities.User.update({
    where: { id: args.userId },
    data: {
      password: 'New pwd which will be hashed automatically!'
    }
  })
}
```
</TabItem>
<TabItem value="ts" label="TypeScript">

```ts title="src/server/actions.ts"
import type { UpdatePassword } from "@wasp/actions/types"
import type { User } from "@wasp/entities"

type UpdatePasswordPayload = {
  userId: User["id"]
}

export const updatePassword: UpdatePassword<UpdatePasswordPayload, User> = async (args, context) => {
  return context.entities.User.update({
    where: { id: args.userId },
    data: {
      password: 'New pwd which will be hashed automatically!'
    }
  })
}
```
</TabItem>
</Tabs>

### Default validations

We provide basic validations out of the box, which you can customize as shown below. 

Default validations depend on the auth method you use.

#### Username & password

If you use the [Username & password](docs/auth/username-and-pass) authentication, the default validations are:
- `username`: non-empty
- `password`: non-empty, at least 8 characters, and contains a number

Note that `username`s are stored in a **case-sensitive** manner.

#### Email

If you use the [Email](docs/auth/email) authentication, the default validations are:
- `email`: non-empty, valid e-mail address
- `password`: non-empty, at least 8 characters, and contains a number

Note that `email`s are stored in a **case-insensitive** manner.

### Customizing validations

:::note
Default validation can be disabled only for the **Username & password** auth method, but the custom validations can be added to both **Username & password** and **Email** auth methods.

This is a bug in Wasp that is being tracked [here](https://github.com/wasp-lang/wasp/issues/1358)
:::

To disable/enable default validations, or add your own, you can modify your custom signup function like so:

<Tabs groupId="js-ts">
<TabItem value="js" label="JavaScript">

```js
const newUser = context.entities.User.create({
  data: {
    username: args.username,
    password: args.password // password hashed automatically by Wasp! 🐝
  },
  _waspSkipDefaultValidations: false, // can be omitted if false (default), or explicitly set to true
  _waspCustomValidations: [
    {
      validates: 'password',
      message: 'password must contain an uppercase letter',
      validator: password => /[A-Z]/.test(password)
    },
  ]
})
```
</TabItem>
<TabItem value="ts" label="TypeScript">

```ts
const newUser = context.entities.User.create({
  data: {
    username: args.username,
    password: args.password // password hashed automatically by Wasp! 🐝
  },
  _waspSkipDefaultValidations: false, // can be omitted if false (default), or explicitly set to true
  _waspCustomValidations: [
    {
      validates: 'password',
      message: 'password must contain an uppercase letter',
      validator: password => /[A-Z]/.test(password)
    },
  ]
})
```
</TabItem>
</Tabs>

:::info
Validations always run on `create()`, but for `update()` they only run when the field mentioned in `validates` is present.

The validation process stops on the first `validator` to return false. If enabled, default validations run first and then custom validations.
:::

### Validation Error Handling
When creating, updating, or deleting entities, you may wish to handle validation errors. We have exposed a class called `AuthError` for this purpose.

<Tabs groupId="js-ts">
<TabItem value="js" label="JavaScript">

```js title="src/server/actions.js"
try {
  await context.entities.User.update(...)
} catch (e) {
  if (e instanceof AuthError) {
    throw new HttpError(422, 'Validation failed', { message: e.message })
  } else {
    throw e
  }
}
```

</TabItem>
<TabItem value="ts" label="TypeScript">

```ts title="src/server/actions.ts"
try {
  await context.entities.User.update(...)
} catch (e) {
  if (e instanceof AuthError) {
    throw new HttpError(422, 'Validation failed', { message: e.message })
  } else {
    throw e
  }
}
```

</TabItem>
</Tabs>

## Options Reference

<Tabs groupId="js-ts">
<TabItem value="js" label="JavaScript">

```wasp
app MyApp {
  title: "My app",
  //...
  auth: {
    userEntity: User,
    externalAuthEntity: SocialLogin,
    methods: {
      usernameAndPassword: {}, // use this or email, not both
      email: {}, // use this or usernameAndPassword, not both
      google: {},
      gitHub: {},
    },
    onAuthFailedRedirectTo: "/someRoute"
  }
}

//...
```
</TabItem>
<TabItem value="ts" label="TypeScript">

```wasp
app MyApp {
  title: "My app",
  //...
  auth: {
    userEntity: User,
    externalAuthEntity: SocialLogin,
    methods: {
      usernameAndPassword: {}, // use this or email, not both
      email: {}, // use this or usernameAndPassword, not both
      google: {},
      gitHub: {},
    },
    onAuthFailedRedirectTo: "/someRoute"
  }
}

//...
```
</TabItem>
</Tabs>

`app.auth` is a dictionary with following fields:

#### `userEntity: entity` <Required />
The entity that represents the user. The fields it should have depend on the auth method you use.

#### `externalAuthEntity: entity`
Anytime an authentication method is used that relies on an external authorization provider, for example, Google, we require an `externalAuthEntity` specified in `auth`, in addition to the `userEntity`, that contains the following configuration:

<Tabs groupId="js-ts">
<TabItem value="js" label="JavaScript">

```wasp {4,14}
//...
  auth: {
    userEntity: User,
    externalAuthEntity: SocialLogin,
//...

entity User {=psl
    id                        Int           @id @default(autoincrement())
    //...
    externalAuthAssociations  SocialLogin[]
psl=}

entity SocialLogin {=psl
  id          Int       @id @default(autoincrement())
  provider    String
  providerId  String
  user        User      @relation(fields: [userId], references: [id], onDelete: Cascade)
  userId      Int
  createdAt   DateTime  @default(now())
  @@unique([provider, providerId, userId])
psl=}
```
</TabItem>
<TabItem value="ts" label="TypeScript">

```wasp {4,14}
//...
  auth: {
    userEntity: User,
    externalAuthEntity: SocialLogin,
//...

entity User {=psl
    id                        Int           @id @default(autoincrement())
    //...
    externalAuthAssociations  SocialLogin[]
psl=}

entity SocialLogin {=psl
  id          Int       @id @default(autoincrement())
  provider    String
  providerId  String
  user        User      @relation(fields: [userId], references: [id], onDelete: Cascade)
  userId      Int
  createdAt   DateTime  @default(now())
  @@unique([provider, providerId, userId])
psl=}
```
</TabItem>
</Tabs>

:::note
The same `externalAuthEntity` can be used across different social login providers (e.g., both GitHub and Google can use the same entity).
:::

See [Google docs](/docs/auth/google) and [GitHub docs](/docs/auth/github) for more details.

#### `methods: dict` <Required />
A dictionary of auth methods that are enabled for the app.

<AuthMethodsGrid />

#### `onAuthFailedRedirectTo: String` <Required />
The path where an unauthenticated user will be redirected if they try to access a private page (which is declared by setting `authRequired: true` for a specific page).
Check out these [essentials docs on auth](/docs/essentials/auth#adding-auth-to-the-project) to see an example of usage.

#### `onAuthSucceededRedirectTo: String`
The path where a successfully authenticated user will be sent upon successful login/signup.
The default value is `"/"`.

:::note
Automatic redirect on successful login only works when using the Wasp provided [`Signup` and `Login` forms](#high-level-api)
:::
