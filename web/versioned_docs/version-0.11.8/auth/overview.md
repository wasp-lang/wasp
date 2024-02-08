---
title: Using Auth
---

import { AuthMethodsGrid } from "@site/src/components/AuthMethodsGrid";
import { Required } from "@site/src/components/Tag";

Auth is an essential piece of any serious application. Coincidentally, Wasp provides authentication and authorization support out of the box.

Here's a 1-minute tour of how full-stack auth works in Wasp:

<div className='video-container'>
    <iframe src="https://www.youtube.com/embed/Qiro77q-ulI?si=y8Rejsbjb1HJC6FA" frameborder="1" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture; web-share" allowfullscreen></iframe>
</div>

Enabling auth for your app is optional and can be done by configuring the `auth` field of the `app` declaration.

<Tabs groupId="js-ts">
<TabItem value="js" label="JavaScript">

```wasp title="main.wasp"
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

```wasp title="main.wasp"
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

Read more about the `auth` field options in the [API Reference](#api-reference) section.

</small>

We will provide a quick overview of auth in Wasp and link to more detailed documentation for each auth method.

## Available auth methods

Wasp supports the following auth methods:

<AuthMethodsGrid />

Let's say we enabled the [Username & password](../auth/username-and-pass) authentication.

We get an auth backend with signup and login endpoints. We also get the `user` object in our [Operations](../data-model/operations/overview) and we can decide what to do based on whether the user is logged in or not.

We would also get the [Auth UI](../auth/ui) generated for us. We can set up our login and signup pages where our users can **create their account** and **login**. We can then protect certain pages by setting `authRequired: true` for them. This will make sure that only logged-in users can access them.

We will also have access to the `user` object in our frontend code, so we can show different UI to logged-in and logged-out users. For example, we can show the user's name in the header alongside a **logout button** or a login button if the user is not logged in.

## Protecting a page with `authRequired`

When declaring a page, you can set the `authRequired` property.

If you set it to `true`, only authenticated users can access the page. Unauthenticated users are redirected to a route defined by the `app.auth.onAuthFailedRedirectTo` field.

<Tabs groupId="js-ts">
<TabItem value="js" label="JavaScript">

```wasp title="main.wasp"
page MainPage {
  component: import Main from "@client/pages/Main.jsx",
  authRequired: true
}
```

</TabItem>
<TabItem value="ts" label="TypeScript">

```wasp title="main.wasp"
page MainPage {
  component: import Main from "@client/pages/Main.tsx",
  authRequired: true
}
```

</TabItem>
</Tabs>

:::caution Requires auth method
You can only use `authRequired` if your app uses one of the [available auth methods](#available-auth-methods).
:::

If `authRequired` is set to `true`, the page's React component (specified by the `component` property) receives the `user` object as a prop. Read more about the `user` object in the [Accessing the logged-in user section](#accessing-the-logged-in-user).

## Logout action

We provide an action for logging out the user. Here's how you can use it:

<Tabs groupId="js-ts">
<TabItem value="js" label="JavaScript">

```jsx title="client/components/LogoutButton.jsx"
import logout from '@wasp/auth/logout'

const LogoutButton = () => {
  return <button onClick={logout}>Logout</button>
}
```

</TabItem>
<TabItem value="ts" label="TypeScript">

```tsx title="client/components/LogoutButton.tsx"
import logout from '@wasp/auth/logout'

const LogoutButton = () => {
  return <button onClick={logout}>Logout</button>
}
```

</TabItem>
</Tabs>

## Accessing the logged-in user

You can get access to the `user` object both in the backend and on the frontend.

### On the client

There are two ways to access the `user` object on the client:

- the `user` prop
- the `useAuth` hook

#### Using the `user` prop

If the page's declaration sets `authRequired` to `true`, the page's React component receives the `user` object as a prop:

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
import Button from './Button'
import logout from '@wasp/auth/logout'

const AccountPage = ({ user }) => {
  return (
    <div>
      <Button onClick={logout}>Logout</Button>
      {JSON.stringify(user, null, 2)}
    </div>
  )
}

export default AccountPage
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
import type { User } from '@wasp/entities'
import Button from './Button'
import logout from '@wasp/auth/logout'

const AccountPage = ({ user }: { user: User }) => {
  return (
    <div>
      <Button onClick={logout}>Logout</Button>
      {JSON.stringify(user, null, 2)}
    </div>
  )
}

export default AccountPage
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
        Please <Link to="/login">login</Link> or{' '}
        <Link to="/signup">sign up</Link>.
      </span>
    )
  } else {
    return (
      <>
        <button onClick={logout}>Logout</button>
        <Todo />
      </>
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

:::tip
Since the `user` prop is only available in a page's React component: use the `user` prop in the page's React component and the `useAuth` hook in any other React component.
:::

### On the server

#### Using the `context.user` object

When authentication is enabled, all [queries and actions](../data-model/operations/overview) have access to the `user` object through the `context` argument. `context.user` contains all User entity's fields, except for the password.

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
        connect: { id: context.user.id },
      },
    },
  })
}
```

</TabItem>
<TabItem value="ts" label="TypeScript">

```ts title="src/server/actions.ts"
import type { Task } from '@wasp/entities'
import type { CreateTask } from '@wasp/actions/types'
import HttpError from '@wasp/core/HttpError.js'

type CreateTaskPayload = Pick<Task, 'description'>

export const createTask: CreateTask<CreateTaskPayload, Task> = async (
  args,
  context
) => {
  if (!context.user) {
    throw new HttpError(403)
  }

  const Task = context.entities.Task
  return Task.create({
    data: {
      description: args.description,
      user: {
        connect: { id: context.user.id },
      },
    },
  })
}
```

</TabItem>
</Tabs>

To implement access control in your app, each operation must check `context.user` and decide what to do. For example, if `context.user` is `undefined` inside a private operation, the user's access should be denied.

When using WebSockets, the `user` object is also available on the `socket.data` object. Read more in the [WebSockets section](../advanced/web-sockets#websocketfn-function).

## User entity

### Password hashing

You don't need to worry about hashing the password yourself. Even when directly using the Prisma client and calling `create()` with a plain-text password, Wasp's middleware makes sure to hash the password before storing it in the database.
For example, if you need to update a user's password, you can safely use the Prisma client to do so, e.g., inside an Action:

<Tabs groupId="js-ts">
<TabItem value="js" label="JavaScript">

```js title="src/server/actions.js"
export const updatePassword = async (args, context) => {
  return context.entities.User.update({
    where: { id: args.userId },
    data: {
      password: 'New pwd which will be hashed automatically!',
    },
  })
}
```

</TabItem>
<TabItem value="ts" label="TypeScript">

```ts title="src/server/actions.ts"
import type { UpdatePassword } from '@wasp/actions/types'
import type { User } from '@wasp/entities'

type UpdatePasswordPayload = {
  userId: User['id']
}

export const updatePassword: UpdatePassword<
  UpdatePasswordPayload,
  User
> = async (args, context) => {
  return context.entities.User.update({
    where: { id: args.userId },
    data: {
      password: 'New pwd which will be hashed automatically!',
    },
  })
}
```

</TabItem>
</Tabs>

### Default validations

Wasp includes several basic validation mechanisms. If you need something extra, the [next section](#customizing-validations) shows how to customize them.

Default validations depend on the auth method you use.

#### Username & password

If you use [Username & password](../auth/username-and-pass) authentication, the default validations are:

- The `username` must not be empty
- The `password` must not be empty, have at least 8 characters, and contain a number

Note that `username`s are stored in a **case-sensitive** manner.

#### Email

If you use [Email](../auth/email) authentication, the default validations are:

- The `email` must not be empty and a valid email address
- The `password` must not be empty, have at least 8 characters, and contain a number

Note that `email`s are stored in a **case-insensitive** manner.

### Customizing validations

:::note
You can only disable the default validation for **Username & password** authentication, but you can add custom validations can to both **Username & password** and **Email** auth methods.

This is a bug in Wasp that is being tracked [here](https://github.com/wasp-lang/wasp/issues/1358)
:::

To disable/enable default validations, or add your own, modify your custom signup function:

<Tabs groupId="js-ts">
<TabItem value="js" label="JavaScript">

```js
const newUser = context.entities.User.create({
  data: {
    username: args.username,
    password: args.password, // password hashed automatically by Wasp! 🐝
  },
  _waspSkipDefaultValidations: false, // can be omitted if false (default), or explicitly set to true
  _waspCustomValidations: [
    {
      validates: 'password',
      message: 'password must contain an uppercase letter',
      validator: (password) => /[A-Z]/.test(password),
    },
  ],
})
```

</TabItem>
<TabItem value="ts" label="TypeScript">

```ts
const newUser = context.entities.User.create({
  data: {
    username: args.username,
    password: args.password, // password hashed automatically by Wasp! 🐝
  },
  _waspSkipDefaultValidations: false, // can be omitted if false (default), or explicitly set to true
  _waspCustomValidations: [
    {
      validates: 'password',
      message: 'password must contain an uppercase letter',
      validator: (password) => /[A-Z]/.test(password),
    },
  ],
})
```

</TabItem>
</Tabs>

:::info
Validations always run on `create()`.
For `update()`, they only run when the field mentioned in `validates` is present.

The validation process stops on the first `validator` to return false. If enabled, default validations run first and then custom validations.
:::

### Validation Error Handling

When creating, updating, or deleting entities, you may wish to handle validation errors. Wasp exposes a class called `AuthError` for this purpose.

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

## Customizing the Signup Process

Sometimes you want to include **extra fields** in your signup process, like first name and last name.

In Wasp, in this case:

- you need to define the fields that you want saved in the database,
- you need to customize the `SignupForm`.

Other times, you might need to just add some **extra UI** elements to the form, like a checkbox for terms of service. In this case, customizing only the UI components is enough.

Let's see how to do both.

### 1. Defining Extra Fields

If we want to **save** some extra fields in our signup process, we need to tell our app they exist.

We do that by defining an object where the keys represent the field name, and the values are functions that receive the data sent from the client\* and return the value of the field.

<small>

\* We exclude the `password` field from this object to prevent it from being saved as plain-text in the database. The `password` field is handled by Wasp's auth backend.
</small>

First, we add the `auth.signup.additionalFields` field in our `main.wasp` file:

<Tabs groupId="js-ts">
<TabItem value="js" label="JavaScript">

```wasp title="main.wasp" {9-11}
app crudTesting {
  // ...
  auth: {
    userEntity: User,
    methods: {
      usernameAndPassword: {},
    },
    onAuthFailedRedirectTo: "/login",
    signup: {
      additionalFields: import { fields } from "@server/auth/signup.js",
    },
  },
}

entity User {=psl
  id Int @id @default(autoincrement())
  username String @unique
  password String
  address String?
psl=}
```

Then we'll define and export the `fields` object from the `server/auth/signup.js` file:

```ts title="server/auth/signup.js"
import { defineAdditionalSignupFields } from '@wasp/auth/index.js'

export const fields = defineAdditionalSignupFields({
  address: async (data) => {
    const address = data.address
    if (typeof address !== 'string') {
      throw new Error('Address is required')
    }
    if (address.length < 5) {
      throw new Error('Address must be at least 5 characters long')
    }
    return address
  },
})
```

</TabItem>
<TabItem value="ts" label="TypeScript">

```wasp title="main.wasp" {9-11}
app crudTesting {
  // ...
  auth: {
    userEntity: User,
    methods: {
      usernameAndPassword: {},
    },
    onAuthFailedRedirectTo: "/login",
    signup: {
      additionalFields: import { fields } from "@server/auth/signup.js",
    },
  },
}

entity User {=psl
  id Int @id @default(autoincrement())
  username String @unique
  password String
  address String?
psl=}
```

Then we'll export the `fields` object from the `server/auth/signup.ts` file:

```ts title="server/auth/signup.ts"
import { defineAdditionalSignupFields } from '@wasp/auth/index.js'

export const fields = defineAdditionalSignupFields({
  address: async (data) => {
    const address = data.address
    if (typeof address !== 'string') {
      throw new Error('Address is required')
    }
    if (address.length < 5) {
      throw new Error('Address must be at least 5 characters long')
    }
    return address
  },
})
```

</TabItem>
</Tabs>

<small>

Read more about the `fields` object in the [API Reference](#signup-fields-customization).
</small>

Keep in mind, that these field names need to exist on the `userEntity` you defined in your `main.wasp` file e.g. `address` needs to be a field on the `User` entity.

The field function will receive the data sent from the client and it needs to return the value that will be saved into the database. If the field is invalid, the function should throw an error.

:::info Using Validation Libraries

You can use any validation library you want to validate the fields. For example, you can use `zod` like this:

<details>
<summary>Click to see the code</summary>

<Tabs groupId="js-ts">
<TabItem value="js" label="JavaScript">

```js title="server/auth/signup.js"
import { defineAdditionalSignupFields } from '@wasp/auth/index.js'
import * as z from 'zod'

export const fields = defineAdditionalSignupFields({
  address: (data) => {
    const AddressSchema = z
      .string({
        required_error: 'Address is required',
        invalid_type_error: 'Address must be a string',
      })
      .min(10, 'Address must be at least 10 characters long')
    const result = AddressSchema.safeParse(data.address)
    if (result.success === false) {
      throw new Error(result.error.issues[0].message)
    }
    return result.data
  },
})
```

</TabItem>
<TabItem value="ts" label="TypeScript">

```ts title="server/auth/signup.ts"
import { defineAdditionalSignupFields } from '@wasp/auth/index.js'
import * as z from 'zod'

export const fields = defineAdditionalSignupFields({
  address: (data) => {
    const AddressSchema = z
      .string({
        required_error: 'Address is required',
        invalid_type_error: 'Address must be a string',
      })
      .min(10, 'Address must be at least 10 characters long')
    const result = AddressSchema.safeParse(data.address)
    if (result.success === false) {
      throw new Error(result.error.issues[0].message)
    }
    return result.data
  },
})
```

</TabItem>
</Tabs>
</details>

:::

Now that we defined the fields, Wasp knows how to:

1. Validate the data sent from the client
2. Save the data to the database

Next, let's see how to customize [Auth UI](../auth/ui) to include those fields.

### 2. Customizing the Signup Component

:::tip Using Custom Signup Component

If you are not using Wasp's Auth UI, you can skip this section. Just make sure to include the extra fields in your custom signup form.

Read more about using the signup actions for:

- email auth [here](../auth/email#fields-in-the-email-dict) <!-- TODO: these docs are not great at explaining using signup and login actions: https://github.com/wasp-lang/wasp/issues/1438 -->
- username & password auth [here](../auth/username-and-pass#customizing-the-auth-flow)
:::

If you are using Wasp's Auth UI, you can customize the `SignupForm` component by passing the `additionalFields` prop to it. It can be either a list of extra fields or a render function.

#### Using a List of Extra Fields

When you pass in a list of extra fields to the `SignupForm`, they are added to the form one by one, in the order you pass them in.

Inside the list, there can be either **objects** or **render functions** (you can combine them):

1. Objects are a simple way to describe new fields you need, but a bit less flexible than render functions.
2. Render functions can be used to render any UI you want, but they require a bit more code. The render functions receive the `react-hook-form` object and the form state object as arguments.

<Tabs groupId="js-ts">
<TabItem value="js" label="JavaScript">

```jsx title="client/SignupPage.jsx"
import { SignupForm } from '@wasp/auth/forms/Signup'
import {
  FormError,
  FormInput,
  FormItemGroup,
  FormLabel,
} from '@wasp/auth/forms/internal/Form'

export const SignupPage = () => {
  return (
    <SignupForm
      additionalFields={[
        /* The address field is defined using an object */
        {
          name: 'address',
          label: 'Address',
          type: 'input',
          validations: {
            required: 'Address is required',
          },
        },
        /* The phone number is defined using a render function */
        (form, state) => {
          return (
            <FormItemGroup>
              <FormLabel>Phone Number</FormLabel>
              <FormInput
                {...form.register('phoneNumber', {
                  required: 'Phone number is required',
                })}
                disabled={state.isLoading}
              />
              {form.formState.errors.phoneNumber && (
                <FormError>
                  {form.formState.errors.phoneNumber.message}
                </FormError>
              )}
            </FormItemGroup>
          )
        },
      ]}
    />
  )
}
```

</TabItem>
<TabItem value="ts" label="TypeScript">

```tsx title="client/SignupPage.tsx"
import { SignupForm } from '@wasp/auth/forms/Signup'
import {
  FormError,
  FormInput,
  FormItemGroup,
  FormLabel,
} from '@wasp/auth/forms/internal/Form'

export const SignupPage = () => {
  return (
    <SignupForm
      additionalFields={[
        /* The address field is defined using an object */
        {
          name: 'address',
          label: 'Address',
          type: 'input',
          validations: {
            required: 'Address is required',
          },
        },
        /* The phone number is defined using a render function */
        (form, state) => {
          return (
            <FormItemGroup>
              <FormLabel>Phone Number</FormLabel>
              <FormInput
                {...form.register('phoneNumber', {
                  required: 'Phone number is required',
                })}
                disabled={state.isLoading}
              />
              {form.formState.errors.phoneNumber && (
                <FormError>
                  {form.formState.errors.phoneNumber.message}
                </FormError>
              )}
            </FormItemGroup>
          )
        },
      ]}
    />
  )
}
```

</TabItem>
</Tabs>

<small>

Read more about the extra fields in the [API Reference](#signupform-customization).
</small>

#### Using a Single Render Function

Instead of passing in a list of extra fields, you can pass in a render function which will receive the `react-hook-form` object and the form state object as arguments. What ever the render function returns, will be rendered below the default fields.

<Tabs groupId="js-ts">
<TabItem value="js" label="JavaScript">

```jsx title="client/SignupPage.jsx"
import { SignupForm } from '@wasp/auth/forms/Signup'
import { FormItemGroup } from '@wasp/auth/forms/internal/Form'

export const SignupPage = () => {
  return (
    <SignupForm
      additionalFields={(form, state) => {
        const username = form.watch('username')
        return (
          username && (
            <FormItemGroup>
              Hello there <strong>{username}</strong> 👋
            </FormItemGroup>
          )
        )
      }}
    />
  )
}
```

</TabItem>
<TabItem value="ts" label="TypeScript">

```tsx title="client/SignupPage.tsx"
import { SignupForm } from '@wasp/auth/forms/Signup'
import { FormItemGroup } from '@wasp/auth/forms/internal/Form'

export const SignupPage = () => {
  return (
    <SignupForm
      additionalFields={(form, state) => {
        const username = form.watch('username')
        return (
          username && (
            <FormItemGroup>
              Hello there <strong>{username}</strong> 👋
            </FormItemGroup>
          )
        )
      }}
    />
  )
}
```

</TabItem>
</Tabs>

<small>

Read more about the render function in the [API Reference](#signupform-customization).
</small>

## API Reference

### Auth Fields

<Tabs groupId="js-ts">
<TabItem value="js" label="JavaScript">

```wasp title="main.wasp"
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
    onAuthFailedRedirectTo: "/someRoute",
    signup: { ... }
  }
}

//...
```

</TabItem>
<TabItem value="ts" label="TypeScript">

```wasp title="main.wasp"
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
    onAuthFailedRedirectTo: "/someRoute",
    signup: { ... }
  }
}

//...
```

</TabItem>
</Tabs>

`app.auth` is a dictionary with the following fields:

#### `userEntity: entity` <Required />

The entity representing the user. Its mandatory fields depend on your chosen auth method.

#### `externalAuthEntity: entity`

Wasp requires you to set the field `auth.externalAuthEntity` for all authentication methods relying on an external authorizatino provider (e.g., Google). You also need to tweak the Entity referenced by `auth.userEntity`, as shown below.

<Tabs groupId="js-ts">
<TabItem value="js" label="JavaScript">

```wasp {4,14} title="main.wasp"
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

```wasp {4,14} title="main.wasp"
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

See [Google docs](../auth/social-auth/google) and [GitHub docs](../auth/social-auth/github) for more details.

#### `methods: dict` <Required />

A dictionary of auth methods enabled for the app.

<AuthMethodsGrid />

#### `onAuthFailedRedirectTo: String` <Required />

The route to which Wasp should redirect unauthenticated user when they try to access a private page (i.e., a page that has `authRequired: true`).
Check out these [essentials docs on auth](../tutorial/auth#adding-auth-to-the-project) to see an example of usage.

#### `onAuthSucceededRedirectTo: String`

The route to which Wasp will send a successfully authenticated after a successful login/signup.
The default value is `"/"`.

:::note
Automatic redirect on successful login only works when using the Wasp-provided [Auth UI](../auth/ui).
:::

#### `signup: SignupOptions`

Read more about the signup process customization API in the [Signup Fields Customization](#signup-fields-customization) section.

### Signup Fields Customization

If you want to add extra fields to the signup process, the server needs to know how to save them to the database. You do that by defining the `auth.signup.additionalFields` field in your `main.wasp` file.

<Tabs groupId="js-ts">
<TabItem value="js" label="JavaScript">

```wasp title="main.wasp" {9-11}
app crudTesting {
  // ...
  auth: {
    userEntity: User,
    methods: {
      usernameAndPassword: {},
    },
    onAuthFailedRedirectTo: "/login",
    signup: {
      additionalFields: import { fields } from "@server/auth/signup.js",
    },
  },
}
```

Then we'll export the `fields` object from the `server/auth/signup.js` file:

```ts title="server/auth/signup.js"
import { defineAdditionalSignupFields } from '@wasp/auth/index.js'

export const fields = defineAdditionalSignupFields({
  address: async (data) => {
    const address = data.address
    if (typeof address !== 'string') {
      throw new Error('Address is required')
    }
    if (address.length < 5) {
      throw new Error('Address must be at least 5 characters long')
    }
    return address
  },
})
```

</TabItem>
<TabItem value="ts" label="TypeScript">

```wasp title="main.wasp" {9-11}
app crudTesting {
  // ...
  auth: {
    userEntity: User,
    methods: {
      usernameAndPassword: {},
    },
    onAuthFailedRedirectTo: "/login",
    signup: {
      additionalFields: import { fields } from "@server/auth/signup.js",
    },
  },
}
```

Then we'll export the `fields` object from the `server/auth/signup.ts` file:

```ts title="server/auth/signup.ts"
import { defineAdditionalSignupFields } from '@wasp/auth/index.js'

export const fields = defineAdditionalSignupFields({
  address: async (data) => {
    const address = data.address
    if (typeof address !== 'string') {
      throw new Error('Address is required')
    }
    if (address.length < 5) {
      throw new Error('Address must be at least 5 characters long')
    }
    return address
  },
})
```

</TabItem>
</Tabs>

The `fields` object is an object where the keys represent the field name, and the values are functions which receive the data sent from the client\* and return the value of the field.

If the field value is invalid, the function should throw an error.

<small>

\* We exclude the `password` field from this object to prevent it from being saved as plain-text in the database. The `password` field is handled by Wasp's auth backend.
</small>

### `SignupForm` Customization

To customize the `SignupForm` component, you need to pass in the `additionalFields` prop. It can be either a list of extra fields or a render function.

<Tabs groupId="js-ts">
<TabItem value="js" label="JavaScript">

```jsx title="client/SignupPage.jsx"
import { SignupForm } from '@wasp/auth/forms/Signup'
import {
  FormError,
  FormInput,
  FormItemGroup,
  FormLabel,
} from '@wasp/auth/forms/internal/Form'

export const SignupPage = () => {
  return (
    <SignupForm
      additionalFields={[
        {
          name: 'address',
          label: 'Address',
          type: 'input',
          validations: {
            required: 'Address is required',
          },
        },
        (form, state) => {
          return (
            <FormItemGroup>
              <FormLabel>Phone Number</FormLabel>
              <FormInput
                {...form.register('phoneNumber', {
                  required: 'Phone number is required',
                })}
                disabled={state.isLoading}
              />
              {form.formState.errors.phoneNumber && (
                <FormError>
                  {form.formState.errors.phoneNumber.message}
                </FormError>
              )}
            </FormItemGroup>
          )
        },
      ]}
    />
  )
}
```

</TabItem>
<TabItem value="ts" label="TypeScript">

```tsx title="client/SignupPage.tsx"
import { SignupForm } from '@wasp/auth/forms/Signup'
import {
  FormError,
  FormInput,
  FormItemGroup,
  FormLabel,
} from '@wasp/auth/forms/internal/Form'

export const SignupPage = () => {
  return (
    <SignupForm
      additionalFields={[
        {
          name: 'address',
          label: 'Address',
          type: 'input',
          validations: {
            required: 'Address is required',
          },
        },
        (form, state) => {
          return (
            <FormItemGroup>
              <FormLabel>Phone Number</FormLabel>
              <FormInput
                {...form.register('phoneNumber', {
                  required: 'Phone number is required',
                })}
                disabled={state.isLoading}
              />
              {form.formState.errors.phoneNumber && (
                <FormError>
                  {form.formState.errors.phoneNumber.message}
                </FormError>
              )}
            </FormItemGroup>
          )
        },
      ]}
    />
  )
}
```

</TabItem>
</Tabs>

The extra fields can be either **objects** or **render functions** (you can combine them):

1. Objects are a simple way to describe new fields you need, but a bit less flexible than render functions.

   The objects have the following properties:

   - `name` <Required />
     - the name of the field
   - `label` <Required />

     - the label of the field (used in the UI)

   - `type` <Required />

     - the type of the field, which can be `input` or `textarea`

   - `validations`
     - an object with the validation rules for the field. The keys are the validation names, and the values are the validation error messages. Read more about the available validation rules in the [react-hook-form docs](https://react-hook-form.com/api/useform/register#register).

2. Render functions receive the `react-hook-form` object and the form state as arguments, and they can use them to render arbitrary UI elements.

   The render function has the following signature:

   ```ts
   (form: UseFormReturn, state: FormState) => React.ReactNode
   ```

   - `form` <Required />

     - the `react-hook-form` object, read more about it in the [react-hook-form docs](https://react-hook-form.com/api/useform)
     - you need to use the `form.register` function to register your fields

   - `state` <Required />

     - the form state object which has the following properties:
       - `isLoading: boolean`
         - whether the form is currently submitting
