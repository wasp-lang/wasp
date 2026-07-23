> Fetch the complete documentation index at: https://wasp.sh/llms.txt
---

# Overview

Auth is an essential piece of any serious application. That's why Wasp provides authentication and authorization support out of the box.

Here's a 1-minute tour of how full-stack auth works in Wasp:

Enabling auth for your app is optional and can be done by configuring the `auth` field of your `app` spec:

```ts title="main.wasp.ts"
import { app } from "@wasp.sh/spec"

export default app({
  name: "MyApp",
  wasp: { version: "^0.24" },
  title: "My app",
  head: ["<link rel='icon' href='/favicon.ico' />"],
  auth: {
    userEntity: "User",
    methods: {
      usernameAndPassword: {}, // use this or email, not both
      email: {}, // use this or usernameAndPassword, not both
      google: {},
      gitHub: {},
    },
    onAuthFailedRedirectTo: "/someRoute",
  },
  // ...
})
```

Read more about the `auth` field options in the [API Reference](#api-reference) section.

We will provide a quick overview of auth in Wasp and link to more detailed documentation for each auth method.

## Available auth methods

Wasp supports the following auth methods:

### [Email »](/docs/auth/email)

[Email verification, password reset, etc.](/docs/auth/email)

### [Username & Password »](/docs/auth/username-and-pass)

[The simplest way to get started](/docs/auth/username-and-pass)

### [Google »](/docs/auth/social-auth/google)

[Users sign in with their Google account](/docs/auth/social-auth/google)

### [Github »](/docs/auth/social-auth/github)

[Users sign in with their Github account](/docs/auth/social-auth/github)

### [Keycloak »](/docs/auth/social-auth/keycloak)

[Users sign in with their Keycloak account](/docs/auth/social-auth/keycloak)

### [Slack »](/docs/auth/social-auth/slack)

[Users sign in with their Slack account](/docs/auth/social-auth/slack)

### [Discord »](/docs/auth/social-auth/discord)

[Users sign in with their Discord account](/docs/auth/social-auth/discord)

Click on each auth method for more details.

Let's say we enabled the [Username & password](/docs/auth/username-and-pass) authentication.

We get an auth backend with signup and login endpoints. We also get the `user` object in our [Operations](/docs/data-model/operations/overview) and we can decide what to do based on whether the user is logged in or not.

We would also get the [Auth UI](/docs/auth/ui) generated for us. We can set up our login and signup pages where our users can **create their account** and **login**. We can then protect certain pages by setting `authRequired: true` for them. This will make sure that only logged-in users can access them.

We will also have access to the `user` object in our frontend code, so we can show different UI to logged-in and logged-out users. For example, we can show the user's name in the header alongside a **logout button** or a login button if the user is not logged in.

## Different ways to use auth

When you have decided which auth methods you want to support, you can also choose how you want to present the authorization flows to your users.

#### Generated components

This is the fastest way to ship, with Wasp generating ready-made components for your app. They allow for some customization to make them consistent with your app. You don't need to implement any UI or logic, and they just work.

### [Email »](/docs/auth/email)

### [Username and password »](/docs/auth/username-and-pass)

### [Social Auth »](/docs/auth/social-auth/overview)

#### Make your own UI

Wasp is flexible enough to let you completely customize your login and signup interface. We give you the auth related functions, and you decide how and when to call them. This allows for total customization of the look-and-feel, and the interaction, but it needs a bit more work.

### [Email »](/docs/auth/email/create-your-own-ui)

### [Username and password »](/docs/auth/username-and-pass/create-your-own-ui)

### [Social Auth »](/docs/auth/social-auth/create-your-own-ui)

:::tip
You don't have to choose one *or* the other! Mix-and-match, and use what you need in each moment. For example, you can create a custom signup screen, but use Wasp's generated components for login.
:::

#### Custom login and signup actions

The previously discussed options should cover the vast majority of cases. But, for the few instances where it is not enough, you can [create your own signup flows](/docs/auth/advanced/custom-auth-actions), with completely custom logic. This is not recommended, and reserved for advanced use cases. Please check first if other Wasp features (mainly [auth hooks](/docs/auth/auth-hooks)) can handle your requirements.

## Protecting a page with `authRequired`

When declaring a page, you can set the `authRequired` property.

If you set it to `true`, only authenticated users can access the page. Unauthenticated users are redirected to a route defined by the `auth.onAuthFailedRedirectTo` field.

```ts title="main.wasp.ts"
import { app, page, route } from "@wasp.sh/spec"
import Main from "./src/pages/Main" with { type: "ref" }

export default app({
  // ...
  spec: [
    route("MainRoute", "/", page(Main, { authRequired: true })),
  ],
})
```

:::caution[Requires auth method]
You can only use `authRequired` if your app uses one of the [available auth methods](#available-auth-methods).
:::

If `authRequired` is set to `true`, the page's React component (passed as the first argument to `page(...)`) receives the `user` object as a prop. Read more about the `user` object in the [Accessing the logged-in user section](#accessing-the-logged-in-user).

## Logout action

We provide an action for logging out the user. Here's how you can use it:

```tsx title="src/components/LogoutButton.tsx"
import { logout } from "wasp/client/auth"

const LogoutButton = () => {
  return <button onClick={logout}>Logout</button>
}
```

## Accessing the logged-in user

You can get access to the `user` object both on the server and on the client. The `user` object contains the logged-in user's data.

The `user` object has all the fields that you defined in your `User` entity. In addition to that, it will also contain all the auth-related fields that Wasp stores. This includes things like the `username` or the email verification status. For example, if you have a user that signed up using an email and password, the `user` object might look like this:

```ts
const user = {
  // User data
  id: "cluqsex9500017cn7i2hwsg17",
  address: "Some address",

  // Auth methods specific data
  identities: {
    email: {
      id: "user@app.com",
      isEmailVerified: true,
      emailVerificationSentAt: "2024-04-08T10:06:02.204Z",
      passwordResetSentAt: null,
    },
  },
}
```

You can read more about how the `User` is connected to the rest of the auth system and how you can access the user data in the [Accessing User Data](/docs/auth/entities) section of the docs.

### On the client

There are two ways to access the `user` object on the client:

- the `user` prop
- the `useAuth` hook

#### Getting the `user` in authenticated routes

If the page's spec sets `authRequired` to `true`, the page's React component receives the `user` object as a prop. This is the simplest way to access the user inside an authenticated page:

```ts title="main.wasp.ts"
import { app, page, route } from "@wasp.sh/spec"
import Account from "./src/pages/Account" with { type: "ref" }

export default app({
  // ...
  spec: [
    route("AccountRoute", "/account", page(Account, {
      authRequired: true
    })),
  ],
})
```

```tsx title="src/pages/Account.tsx"
import type { AuthUser } from "wasp/auth";
import Button from "./Button";
import { logout } from "wasp/client/auth";

const AccountPage = ({ user }: { user: AuthUser }) => {
  return (
    <div>
      <Button onClick={logout}>Logout</Button>
      {JSON.stringify(user, null, 2)}
    </div>
  );
};

export default AccountPage;
```

#### Getting the `user` in non-authenticated routes

Wasp provides a React hook you can use in the client components - `useAuth`.

This hook is a thin wrapper over Wasp's `useQuery` hook and returns data in the same format.

```tsx title="src/pages/MainPage.tsx"
import { useAuth, logout } from "wasp/client/auth";
import { Link } from "react-router";
import Todo from "../Todo";

export function Main() {
  const { data: user } = useAuth();

  if (!user) {
    return (
      <span>
        Please <Link to="/login">login</Link> or{" "}
        <Link to="/signup">sign up</Link>.
      </span>
    );
  } else {
    return (
      <>
        <button onClick={logout}>Logout</button>
        <Todo />
      </>
    );
  }
}
```

### On the server

#### Using the `context.user` object

When authentication is enabled, all [queries and actions](/docs/data-model/operations/overview) have access to the `user` object through the `context` argument. `context.user` contains all User entity's fields and the auth identities connected to the user. We strip out the `hashedPassword` field from the identities for security reasons.

```ts title="src/actions.ts"
import type { Task } from "wasp/entities";
import type { CreateTask } from "wasp/server/operations";
import { HttpError } from "wasp/server";

type CreateTaskPayload = Pick<Task, "description">;

export const createTask: CreateTask<CreateTaskPayload, Task> = async (
  args,
  context,
) => {
  if (!context.user) {
    throw new HttpError(403);
  }

  const Task = context.entities.Task;
  return Task.create({
    data: {
      description: args.description,
      user: {
        connect: { id: context.user.id },
      },
    },
  });
};
```

To implement access control in your app, each operation must check `context.user` and decide what to do. For example, if `context.user` is `undefined` inside a private operation, the user's access should be denied.

When using WebSockets, the `user` object is also available on the `socket.data` object. Read more in the [WebSockets section](/docs/advanced/web-sockets#websocketfn).

## Sessions

Wasp's auth uses sessions to keep track of the logged-in user. The session is stored in `localStorage` on the client and in the database on the server. Under the hood, Wasp uses the excellent [Lucia Auth v3](https://v3.lucia-auth.com/) library for session management.

When users log in, Wasp creates a session for them and stores it in the database. The session is then sent to the client and stored in `localStorage`. When users log out, Wasp deletes the session from the database and from `localStorage`.

## User Entity

### Password Hashing

If you are saving a user's password in the database, you should **never** save it as plain text. You can use Wasp's helper functions for serializing and deserializing provider data which will automatically hash the password for you:

```ts title="main.wasp.ts"
import { action, app } from "@wasp.sh/spec"
import { updatePassword } from "./src/auth" with { type: "ref" }

export default app({
  // ...
  spec: [
    action(updatePassword),
  ],
})
```

```ts title="src/auth.ts"
import {
  createProviderId,
  findAuthIdentity,
  updateAuthIdentityProviderData,
  getProviderDataWithPassword,
} from "wasp/server/auth";
import type { UpdatePassword } from "wasp/server/operations";

export const updatePassword: UpdatePassword<
  { email: string; password: string },
  void
> = async (args, context) => {
  const providerId = createProviderId("email", args.email);
  const authIdentity = await findAuthIdentity(providerId);
  if (!authIdentity) {
    throw new HttpError(400, "Unknown user");
  }

  const providerData = getProviderDataWithPassword<"email">(
    authIdentity.providerData,
  );

  // Updates the password and hashes it automatically.
  await updateAuthIdentityProviderData(providerId, providerData, {
    hashedPassword: args.password,
  });
};
```

### Default Validations

When you are using the default authentication flow, Wasp validates the fields with some default validations. These validations run if you use Wasp's built-in [Auth UI](/docs/auth/ui) or if you use the provided auth actions.

If you decide to create your [custom auth actions](/docs/auth/advanced/custom-auth-actions), you'll need to run the validations yourself.

Default validations depend on the auth method you use.

#### Username & Password

If you use [Username & password](/docs/auth/username-and-pass) authentication, the default validations are:

- The `username` must not be empty
- The `password` must not be empty, have at least 8 characters, and contain a number

Note that `username`s are stored in a **case-insensitive** manner.

#### Email

If you use [Email](/docs/auth/email) authentication, the default validations are:

- The `email` must not be empty and a valid email address
- The `password` must not be empty, have at least 8 characters, and contain a number

Note that `email`s are stored in a **case-insensitive** manner.

## Customizing the Signup Process

Sometimes you want to include **extra fields** in your signup process, like first name and last name and save them in the `User` entity.

For this to happen:

- you need to define the fields that you want saved in the database,
- you need to customize the `SignupForm` (in the case of [Email](/docs/auth/email) or [Username & Password](/docs/auth/username-and-pass) auth)

Other times, you might need to just add some **extra UI** elements to the form, like a checkbox for terms of service. In this case, customizing only the UI components is enough.

Let's see how to do both.

### 1. Defining Extra Fields

If we want to **save** some extra fields in our signup process, we need to tell our app they exist.

We do that by defining an object where the keys represent the field name, and the values are functions that receive the data sent from the client\* and return the value of the field.

\* We exclude the `password` field from this object to prevent it from being saved as plain-text in the database. The `password` field is handled by Wasp's auth backend.

First, we add the `auth.methods.{authMethod}.userSignupFields` field in our `main.wasp.ts` file. The `{authMethod}` depends on the auth method you are using.

For example, if you are using [Username & Password](/docs/auth/username-and-pass), you would add the `auth.methods.usernameAndPassword.userSignupFields` field:

```ts title="main.wasp.ts"
import { app } from "@wasp.sh/spec"
import { userSignupFields } from "./src/auth/signup" with { type: "ref" }

export default app({
  name: "myApp",
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
  // ...
})
```

```prisma title="schema.prisma"
model User {
  id      Int     @id @default(autoincrement())
  address String?
}
```

Then we'll define the `userSignupFields` object in the `src/auth/signup.ts` file:

```ts title="src/auth/signup.ts"
import { defineUserSignupFields } from "wasp/server/auth";

export const userSignupFields = defineUserSignupFields({
  address: async (data) => {
    const address = data.address;
    if (typeof address !== "string") {
      throw new Error("Address is required");
    }
    if (address.length < 5) {
      throw new Error("Address must be at least 5 characters long");
    }
    return address;
  },
});
```

Read more about the `userSignupFields` object in the [API Reference](#signup-fields-customization).

Keep in mind, that these field names need to exist on the `userEntity` you defined in your `main.wasp.ts` file e.g. `address` needs to be a field on the `User` entity you defined in the `schema.prisma` file.

The field function will receive the data sent from the client and it needs to return the value that will be saved into the database. If the field is invalid, the function should throw an error.

:::info[Using Validation Libraries]
You can use any validation library you want to validate the fields. For example, you can use `zod` like this:

Click to see the code

```ts title="src/auth/signup.ts"
import { defineUserSignupFields } from "wasp/server/auth";
import * as z from "zod";

export const userSignupFields = defineUserSignupFields({
  address: (data) => {
    const AddressSchema = z
      .string({
        required_error: "Address is required",
        invalid_type_error: "Address must be a string",
      })
      .min(10, "Address must be at least 10 characters long");
    const result = AddressSchema.safeParse(data.address);
    if (result.success === false) {
      throw new Error(result.error.issues[0].message);
    }
    return result.data;
  },
});
```
:::

Now that we defined the fields, Wasp knows how to:

1. Validate the data sent from the client
2. Save the data to the database

Next, let's see how to customize [Auth UI](/docs/auth/ui) to include those fields.

### 2. Customizing the Signup Component

:::tip[Using Custom Signup Component]
If you are not using Wasp's Auth UI, you can skip this section. Just make sure to include the extra fields in your custom signup form.

Read more about using the signup actions for:

- [Email auth](/docs/auth/email/create-your-own-ui)
- [Username & password auth](/docs/auth/username-and-pass/create-your-own-ui)
:::

If you are using Wasp's Auth UI, you can customize the `SignupForm` component by passing the `additionalFields` prop to it. It can be either a list of extra fields or a render function.

#### Using a List of Extra Fields

When you pass in a list of extra fields to the `SignupForm`, they are added to the form one by one, in the order you pass them in.

Inside the list, there can be either **objects** or **render functions** (you can combine them):

1. Objects are a simple way to describe new fields you need, but a bit less flexible than render functions.
2. Render functions can be used to render any UI you want, but they require a bit more code. The render functions receive the `react-hook-form` object and the form state object as arguments.

```tsx title="src/SignupPage.tsx"
import {
  SignupForm,
  FormError,
  FormInput,
  FormItemGroup,
  FormLabel,
} from "wasp/client/auth";

export const SignupPage = () => {
  return (
    <SignupForm
      additionalFields={[
        /* The address field is defined using an object */
        {
          name: "address",
          label: "Address",
          type: "input",
          validations: {
            required: "Address is required",
          },
        },
        /* The phone number is defined using a render function */
        (form, state) => {
          return (
            <FormItemGroup>
              <FormLabel>Phone Number</FormLabel>
              <FormInput
                {...form.register("phoneNumber", {
                  required: "Phone number is required",
                })}
                disabled={state.isLoading}
              />
              {form.formState.errors.phoneNumber && (
                <FormError>
                  {form.formState.errors.phoneNumber.message}
                </FormError>
              )}
            </FormItemGroup>
          );
        },
      ]}
    />
  );
};
```

Read more about the extra fields in the [API Reference](#signupform-customization).

#### Using a Single Render Function

Instead of passing in a list of extra fields, you can pass in a render function which will receive the `react-hook-form` object and the form state object as arguments. What ever the render function returns, will be rendered below the default fields.

```tsx title="src/SignupPage.tsx"
import { SignupForm, FormItemGroup } from "wasp/client/auth";

export const SignupPage = () => {
  return (
    <SignupForm
      additionalFields={(form, state) => {
        const username = form.watch("username");
        return (
          username && (
            <FormItemGroup>
              Hello there <strong>{username}</strong> 👋
            </FormItemGroup>
          )
        );
      }}
    />
  );
};
```

Read more about the render function in the [API Reference](#signupform-customization).

## API Reference

### Auth Fields

[API reference](/docs/api/@wasp.sh/spec/interfaces/Auth)

### [Auth »](/docs/api/@wasp.sh/spec/interfaces/Auth)

[All the options for the auth field of the app spec.](/docs/api/@wasp.sh/spec/interfaces/Auth)

### Signup Fields Customization

If you want to add extra fields to the signup process, the server needs to know how to save them to the database. You do that by defining the `auth.methods.{authMethod}.userSignupFields` field in your `main.wasp.ts` file.

```ts title="main.wasp.ts"
import { app } from "@wasp.sh/spec"
import { userSignupFields } from "./src/auth/signup" with { type: "ref" }

export default app({
  name: "myApp",
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
  // ...
})
```

Then we'll export the `userSignupFields` object from the `src/auth/signup.ts` file:

```ts title="src/auth/signup.ts"
import { defineUserSignupFields } from "wasp/server/auth";

export const userSignupFields = defineUserSignupFields({
  address: async (data) => {
    const address = data.address;
    if (typeof address !== "string") {
      throw new Error("Address is required");
    }
    if (address.length < 5) {
      throw new Error("Address must be at least 5 characters long");
    }
    return address;
  },
});
```

The `userSignupFields` object is an object where the keys represent the field name, and the values are functions that receive the data sent from the client\* and return the value of the field.

If the value that the function received is invalid, the function should throw an error.

\* We exclude the `password` field from this object to prevent it from being saved as plain text in the database. The `password` field is handled by Wasp's auth backend.

### `SignupForm` Customization

To customize the `SignupForm` component, you need to pass in the `additionalFields` prop. It can be either a list of extra fields or a render function.

```tsx title="src/SignupPage.tsx"
import {
  SignupForm,
  FormError,
  FormInput,
  FormItemGroup,
  FormLabel,
} from "wasp/client/auth";

export const SignupPage = () => {
  return (
    <SignupForm
      additionalFields={[
        {
          name: "address",
          label: "Address",
          type: "input",
          validations: {
            required: "Address is required",
          },
        },
        (form, state) => {
          return (
            <FormItemGroup>
              <FormLabel>Phone Number</FormLabel>
              <FormInput
                {...form.register("phoneNumber", {
                  required: "Phone number is required",
                })}
                disabled={state.isLoading}
              />
              {form.formState.errors.phoneNumber && (
                <FormError>
                  {form.formState.errors.phoneNumber.message}
                </FormError>
              )}
            </FormItemGroup>
          );
        },
      ]}
    />
  );
};
```

The extra fields can be either **objects** or **render functions** (you can combine them):

1. Objects are a simple way to describe new fields you need, but a bit less flexible than render functions.

   The objects have the following properties:

   - `name` required

     - the name of the field

   - `label` required

     - the label of the field (used in the UI)

   - `type` required

     - the type of the field, which can be `input` or `textarea`

   - `validations`

     - an object with the validation rules for the field. The keys are the validation names, and the values are the validation error messages. Read more about the available validation rules in the [react-hook-form docs](https://react-hook-form.com/api/useform/register#register).

2. Render functions receive the `react-hook-form` object and the form state as arguments, and they can use them to render arbitrary UI elements.

   The render function has the following signature:

   ```ts
   type AdditionalSignupFieldRenderFn = (
      hookForm: UseFormReturn,
      formState: FormState
    ) => React.ReactNode
   ```

   - `form` required

     The `react-hook-form` object, read more about it in the [react-hook-form docs](https://react-hook-form.com/api/useform). You need to use the `form.register` function to register your fields

   - `state` required

     The form state object, which has the following properties:

     - `isLoading: boolean`

       Whether the form is currently submitting