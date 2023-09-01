---
title: Auth UI
---

import { EmailPill, UsernameAndPasswordPill, GithubPill, GooglePill } from "./Pills";
import { Required } from "@site/src/components/Required";

To make using authentication in your app as easy as possible, Wasp generates the server-side code but also the client-side UI for you. It enables you to quickly get the login, signup, password reset and email verification flows in your app.

Below we cover all of the available UI components and how to use them.

![Auth UI](/img/authui/all_screens.gif)

## Overview

After Wasp generates the UI components for your auth, you can use it as is, or customize it to your liking.

Based on the authentication providers you enabled in your `main.wasp` file, the Auth UI will show the corresponding UI (form and buttons). For example, if you enabled e-mail authentication:

<Tabs groupId="js-ts">
<TabItem value="js" label="JavaScript">

```wasp {5} title="main.wasp"
app MyApp {
  //...
  auth: {
    methods: {
      email: {},
    },
    // ...
  }
}
```

</TabItem>
<TabItem value="ts" label="TypeScript">

```wasp {5} title="main.wasp"
app MyApp {
  //...
  auth: {
    methods: {
      email: {},
    },
    // ...
  }
}
```

</TabItem>
</Tabs>

You'll get the following UI:

![Auth UI](/img/authui/login.png)

And then if you enable Google and Github:

<Tabs groupId="js-ts">
<TabItem value="js" label="JavaScript">

```wasp title="main.wasp" {6-7}
app MyApp {
  //...
  auth: {
    methods: {
      email: {},
      google: {},
      github: {},
    },
    // ...
  }
}
```

</TabItem>
<TabItem value="ts" label="TypeScript">

```wasp title="main.wasp" {6-7}
app MyApp {
  //...
  auth: {
    methods: {
      email: {},
      google: {},
      github: {},
    },
    // ...
  }
}
```

</TabItem>
</Tabs>

The form will automatically update to look like this:

![Auth UI](/img/authui/multiple_providers.png)

Let's go through all of the available components and how to use them.

## Auth Components

The following components are available for you to use in your app:

- [Login form](#login-form)
- [Signup form](#signup-form)
- [Forgot password form](#forgot-password-form)
- [Reset password form](#reset-password-form)
- [Verify email form](#verify-email-form)

### Login Form

Used with <UsernameAndPasswordPill />, <EmailPill />, <GithubPill /> and <GooglePill /> authentication.

![Login form](/img/authui/login.png)

You can use the `LoginForm` component to build your login page:

<Tabs groupId="js-ts">
<TabItem value="js" label="JavaScript">

```wasp title="main.wasp"
// ...

route LoginRoute { path: "/login", to: LoginPage }
page LoginPage {
  component: import { LoginPage } from "@client/LoginPage.jsx"
}
```

```tsx title="client/LoginPage.jsx"
import { LoginForm } from "@wasp/auth/forms/Login";

// Use it like this
export function LoginPage() {
  return <LoginForm />;
}
```

</TabItem>
<TabItem value="ts" label="TypeScript">

```wasp title="main.wasp"
// ...

route LoginRoute { path: "/login", to: LoginPage }
page LoginPage {
  component: import { LoginPage } from "@client/LoginPage.tsx"
}
```

```tsx title="client/LoginPage.tsx"
import { LoginForm } from "@wasp/auth/forms/Login";

// Use it like this
export function LoginPage() {
  return <LoginForm />;
}
```

</TabItem>
</Tabs>

It will automatically show the correct authentication providers based on your `main.wasp` file.

### Signup Form

Used with <UsernameAndPasswordPill />, <EmailPill />, <GithubPill /> and <GooglePill /> authentication.

![Signup form](/img/authui/signup.png)

You can use the `SignupForm` component to build your signup page:

<Tabs groupId="js-ts">
<TabItem value="js" label="JavaScript">

```wasp title="main.wasp"
// ...

route SignupRoute { path: "/signup", to: SignupPage }
page SignupPage {
  component: import { SignupPage } from "@client/SignupPage.jsx"
}
```

```tsx title="client/SignupPage.jsx"
import { SignupForm } from "@wasp/auth/forms/Signup";

// Use it like this
export function SignupPage() {
  return <SignupForm />;
}
```

</TabItem>
<TabItem value="ts" label="TypeScript">

```wasp title="main.wasp"
// ...

route SignupRoute { path: "/signup", to: SignupPage }
page SignupPage {
  component: import { SignupPage } from "@client/SignupPage.tsx"
}
```

```tsx title="client/SignupPage.tsx"
import { SignupForm } from "@wasp/auth/forms/Signup";

// Use it like this
export function SignupPage() {
  return <SignupForm />;
}
```

</TabItem>
</Tabs>

It will automatically show the correct authentication providers based on your `main.wasp` file.

Read more about [customizing the signup form](#customizing-the-signup-form) below.

### Forgot Password Form

Used with <EmailPill /> authentication.

If users forget their password, they can use this form to reset it.

![Forgot password form](/img/authui/forgot_password.png)

You can use the `ForgotPasswordForm` component to build your own forgot password page:

<Tabs groupId="js-ts">
<TabItem value="js" label="JavaScript">

```wasp title="main.wasp"
// ...

route RequestPasswordResetRoute { path: "/request-password-reset", to: RequestPasswordResetPage }
page RequestPasswordResetPage {
  component: import { ForgotPasswordPage } from "@client/ForgotPasswordPage.jsx"
}
```

```tsx title="client/ForgotPasswordPage.jsx"
import { ForgotPasswordForm } from "@wasp/auth/forms/ForgotPassword";

// Use it like this
export function ForgotPasswordPage() {
  return <ForgotPasswordForm />;
}
```

</TabItem>
<TabItem value="ts" label="TypeScript">

```wasp title="main.wasp"
// ...

route RequestPasswordResetRoute { path: "/request-password-reset", to: RequestPasswordResetPage }
page RequestPasswordResetPage {
  component: import { ForgotPasswordPage } from "@client/ForgotPasswordPage.tsx"
}
```

```tsx title="client/ForgotPasswordPage.tsx"
import { ForgotPasswordForm } from "@wasp/auth/forms/ForgotPassword";

// Use it like this
export function ForgotPasswordPage() {
  return <ForgotPasswordForm />;
}
```

</TabItem>
</Tabs>

### Reset Password Form

Used with <EmailPill /> authentication.

After users click on the link in the email they receive after submitting the forgot password form, they will be redirected to this form where they can reset their password.

![Reset password form](/img/authui/reset_password.png)

You can use the `ResetPasswordForm` component to build your reset password page:

<Tabs groupId="js-ts">
<TabItem value="js" label="JavaScript">

```wasp title="main.wasp"
// ...

route PasswordResetRoute { path: "/password-reset", to: PasswordResetPage }
page PasswordResetPage {
  component: import { ResetPasswordPage } from "@client/ResetPasswordPage.jsx"
}
```

```tsx title="client/ResetPasswordPage.jsx"
import { ResetPasswordForm } from "@wasp/auth/forms/ResetPassword";

// Use it like this
export function ResetPasswordPage() {
  return <ResetPasswordForm />;
}
```

</TabItem>
<TabItem value="ts" label="TypeScript">

```wasp title="main.wasp"
// ...

route PasswordResetRoute { path: "/password-reset", to: PasswordResetPage }
page PasswordResetPage {
  component: import { ResetPasswordPage } from "@client/ResetPasswordPage.tsx"
}
```

```tsx title="client/ResetPasswordPage.tsx"
import { ResetPasswordForm } from "@wasp/auth/forms/ResetPassword";

// Use it like this
export function ResetPasswordPage() {
  return <ResetPasswordForm />;
}
```

</TabItem>
</Tabs>

### Verify Email Form

Used with <EmailPill /> authentication.

After users sign up, they will receive an email with a link to this form where they can verify their email.

![Verify email form](/img/authui/email_verification.png)

You can use the `VerifyEmailForm` component to build your email verification page:

<Tabs groupId="js-ts">
<TabItem value="js" label="JavaScript">

```wasp title="main.wasp"
// ...

route EmailVerificationRoute { path: "/email-verification", to: EmailVerificationPage }
page EmailVerificationPage {
  component: import { VerifyEmailPage } from "@client/VerifyEmailPage.jsx"
}
```

```tsx title="client/VerifyEmailPage.jsx"
import { VerifyEmailForm } from "@wasp/auth/forms/VerifyEmail";

// Use it like this
export function VerifyEmailPage() {
  return <VerifyEmailForm />;
}
```

</TabItem>
<TabItem value="ts" label="TypeScript">

```wasp title="main.wasp"
// ...

route EmailVerificationRoute { path: "/email-verification", to: EmailVerificationPage }
page EmailVerificationPage {
  component: import { VerifyEmailPage } from "@client/VerifyEmailPage.tsx"
}
```

```tsx title="client/VerifyEmailPage.tsx"
import { VerifyEmailForm } from "@wasp/auth/forms/VerifyEmail";

// Use it like this
export function VerifyEmailPage() {
  return <VerifyEmailForm />;
}
```

</TabItem>
</Tabs>

## Customization 💅🏻

You customize all of the available forms by passing props to them.

Props you can pass to all of the forms:

1. `appearance` - customize the form colors (via design tokens)
2. `logo` - path to your logo
3. `socialLayout` - layout of the social buttons, which can be `vertical` or `horizontal`

### 1. Customizing the Colors

We use [Stitches](https://stitches.dev/) to style the Auth UI. You can customize the styles by overriding the default theme tokens.

:::info List of all available tokens

See the [list of all available tokens](https://github.com/wasp-lang/wasp/blob/release/waspc/data/Generator/templates/react-app/src/stitches.config.js) which you can override.

:::

<Tabs groupId="js-ts">
<TabItem value="js" label="JavaScript">

```js title="client/appearance.js"
export const authAppearance = {
  colors: {
    brand: "#5969b8", // blue
    brandAccent: "#de5998", // pink
    submitButtonText: "white",
  },
};
```

```jsx title="client/LoginPage.jsx"
import { LoginForm } from "@wasp/auth/forms/Login";
import { authAppearance } from "./appearance";

export function LoginPage() {
  return (
    <LoginForm
      // Pass the appearance object to the form
      appearance={authAppearance}
    />
  );
}
```

</TabItem>
<TabItem value="ts" label="TypeScript">

```ts title="client/appearance.ts"
import type { CustomizationOptions } from "@wasp/auth/forms/types";

export const authAppearance: CustomizationOptions["appearance"] = {
  colors: {
    brand: "#5969b8", // blue
    brandAccent: "#de5998", // pink
    submitButtonText: "white",
  },
};
```

```tsx title="client/LoginPage.tsx"
import { LoginForm } from "@wasp/auth/forms/Login";
import { authAppearance } from "./appearance";

export function LoginPage() {
  return (
    <LoginForm
      // Pass the appearance object to the form
      appearance={authAppearance}
    />
  );
}
```

</TabItem>
</Tabs>

We recommend defining your appearance in a separate file and importing it into your components.

### 2. Using Your Logo

You can add your logo to the Auth UI by passing the `logo` prop to any of the components.

<Tabs groupId="js-ts">
<TabItem value="js" label="JavaScript">

```tsx title="client/LoginPage.jsx"
import { LoginForm } from "@wasp/auth/forms/Login";
import Logo from "./logo.png";

export function LoginPage() {
  return (
    <LoginForm
      // Pass in the path to your logo
      logo={Logo}
    />
  );
}
```

</TabItem>
<TabItem value="ts" label="TypeScript">

```tsx title="client/LoginPage.tsx"
import { LoginForm } from "@wasp/auth/forms/Login";
import Logo from "./logo.png";

export function LoginPage() {
  return (
    <LoginForm
      // Pass in the path to your logo
      logo={Logo}
    />
  );
}
```

</TabItem>
</Tabs>

### 3. Social Buttons Layout

You can change the layout of the social buttons by passing the `socialLayout` prop to any of the components. It can be either `vertical` or `horizontal` (default).

If we pass in `vertical`:

<Tabs groupId="js-ts">
<TabItem value="js" label="JavaScript">

```tsx title="client/LoginPage.jsx"
import { LoginForm } from "@wasp/auth/forms/Login";

export function LoginPage() {
  return (
    <LoginForm
      // Pass in the socialLayout prop
      socialLayout="vertical"
    />
  );
}
```

</TabItem>
<TabItem value="ts" label="TypeScript">

```tsx title="client/LoginPage.tsx"
import { LoginForm } from "@wasp/auth/forms/Login";

export function LoginPage() {
  return (
    <LoginForm
      // Pass in the socialLayout prop
      socialLayout="vertical"
    />
  );
}
```

</TabItem>
</Tabs>

We get this:

![Vertical social buttons](/img/authui/vertical_social_buttons.png)

### Let's Put Everything Together 🪄

If we provide the logo and custom colors:

<Tabs groupId="js-ts">
<TabItem value="js" label="JavaScript">

```ts title="client/appearance.js"
export const appearance = {
  colors: {
    brand: "#5969b8", // blue
    brandAccent: "#de5998", // pink
    submitButtonText: "white",
  },
};
```

```tsx title="client/LoginPage.jsx"
import { LoginForm } from "@wasp/auth/forms/Login";

import { authAppearance } from "./appearance";
import todoLogo from "./todoLogo.png";

export function LoginPage() {
  return <LoginForm appearance={appearance} logo={todoLogo} />;
}
```

</TabItem>
<TabItem value="ts" label="TypeScript">

```ts title="client/appearance.ts"
import type { CustomizationOptions } from "@wasp/auth/forms/types";

export const appearance: CustomizationOptions["appearance"] = {
  colors: {
    brand: "#5969b8", // blue
    brandAccent: "#de5998", // pink
    submitButtonText: "white",
  },
};
```

```tsx title="client/LoginPage.tsx"
import { LoginForm } from "@wasp/auth/forms/Login";

import { authAppearance } from "./appearance";
import todoLogo from "./todoLogo.png";

export function LoginPage() {
  return <LoginForm appearance={appearance} logo={todoLogo} />;
}
```

</TabItem>
</Tabs>

We get a form looking like this:

<div style={{ textAlign: 'center' }}>
  <img src="/img/authui/custom_login.gif" alt="Custom login form" />
</div>

## Customizing the Signup Form

Sometimes you want to include **extra fields** in your signup process, like first name and last name.

In Wasp, in this case:

- You need to define the new backend fields
- You need to customize the `SignupForm`

Other times, you might need to add some **extra UI** elements to the form, like a checkbox for terms of service. In this case, customizing only the UI components is enough.

Let's see how to do both.

### 1. Defining Extra Backend Fields

If we want to **save** some extra fields in our signup process, we need to tell our backend they exist.

We do that by defining an object where the keys represent the field name, and the values are objects with the field details. Keep in mind, that these field names need to exist on the `userEntity` you defined in your `main.wasp` file.

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
```

Then we'll export the `fields` object from the `server/auth/signup.js` file:

```ts title="server/auth/signup.js"
import { defineAdditionalSignupFields } from "@wasp/auth/index.js";

export const fields = defineAdditionalSignupFields({
  address: {
    get: (data) => {
      return data.address;
    },
    validate: (address) => {
      if (!address) {
        throw new Error("Address is required");
      }
      if (address.length < 5) {
        throw new Error("Address must be at least 5 characters long");
      }
    },
  },
});
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
import { defineAdditionalSignupFields } from "@wasp/auth/index.js";

export const fields = defineAdditionalSignupFields({
  address: {
    get: (data) => {
      return data.address as string | undefined;
    },
    validate: (address) => {
      if (!address) {
        throw new Error("Address is required");
      }
      if (address.length < 5) {
        throw new Error("Address must be at least 5 characters long");
      }
    },
  },
});
```

</TabItem>
</Tabs>

<small>

Read more about the fields in the [API Reference](#signup-fields-customization).
</small>

Now that we defined the fields, Wasp knows how to:

1. Validate the data sent from the client
2. Save the data to the database

Next, let's see how to customize the UI to include those fields.

### 2. Customizing the Signup Component

You can customize the `SignupForm` component by passing the `additionalFields` prop to it. It can be either a list of extra fields or a render function.

#### Using a List of Extra Fields

When you pass in a list of extra fields to the `SignupForm`, they are added to the form one by one, in the order you pass them in.

Inside the list, there can be either **objects** or **render functions** (you can combine them):

1. Objects are a simple way to describe new fields you need, but a bit less flexible than render functions.
2. Render functions receive the `react-hook-form` object and the form state object as arguments, and they can use them to render arbitrary UI elements.

<Tabs groupId="js-ts">
<TabItem value="js" label="JavaScript">

```jsx title="client/SignupPage.jsx"
import { SignupForm } from "@wasp/auth/forms/Signup";
import {
  FormError,
  FormInput,
  FormItemGroup,
  FormLabel,
} from "@wasp/auth/forms/internal/Form";

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

</TabItem>
<TabItem value="ts" label="TypeScript">

```tsx title="client/SignupPage.tsx"
import { SignupForm } from "@wasp/auth/forms/Signup";
import {
  FormError,
  FormInput,
  FormItemGroup,
  FormLabel,
} from "@wasp/auth/forms/internal/Form";

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

</TabItem>
</Tabs>

<small>

Read more about the extra fields in the [API Reference](#signupform-customization).
</small>

#### Using a Single Render Function

Instead of passing in a list of extra fields, you can pass in a render function which will receive the `react-hook-form` object and the form state object as arguments. You can use them to render arbitrary UI elements.

<Tabs groupId="js-ts">
<TabItem value="js" label="JavaScript">

```jsx title="client/SignupPage.jsx"
import { SignupForm } from "@wasp/auth/forms/Signup";
import { FormItemGroup } from "@wasp/auth/forms/internal/Form";

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

</TabItem>
<TabItem value="ts" label="TypeScript">

```tsx title="client/SignupPage.tsx"
import { SignupForm } from "@wasp/auth/forms/Signup";
import { FormItemGroup } from "@wasp/auth/forms/internal/Form";

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

</TabItem>
</Tabs>

<small>

Read more about the render function in the [API Reference](#signupform-customization).
</small>

## API Reference

### Signup Fields Customization

If you want to add extra fields to the signup process, the backend needs to know how to save them to the database. You do that by defining the `auth.signup.additionalFields` field in your `main.wasp` file.

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
import { defineAdditionalSignupFields } from "@wasp/auth/index.js";

export const fields = defineAdditionalSignupFields({
  address: {
    get: (data) => {
      return data.address;
    },
    validate: (address) => {
      if (!address) {
        throw new Error("Address is required");
      }
      if (address.length < 5) {
        throw new Error("Address must be at least 5 characters long");
      }
    },
  },
});
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

Then you define the `fields` object from the `server/auth/signup.ts` file:

```ts title="server/auth/signup.ts"
import { defineAdditionalSignupFields } from "@wasp/auth/index.js";

export const fields = defineAdditionalSignupFields({
  address: {
    get: (data) => {
      return data.address as string | undefined;
    },
    validate: (address) => {
      if (!address) {
        throw new Error("Address is required");
      }
      if (address.length < 5) {
        throw new Error("Address must be at least 5 characters long");
      }
    },
  },
});
```

</TabItem>
</Tabs>

The `fields` object is an object where the keys represent the field name, and the values are objects with the field details.

The field details object has the following properties:

- `get` <Required />

  - a function which receives the data sent from the client and returns the value of the field

- `validate` <Required />
  - a function which receives the value of the field and throws an error if the value is invalid

### `SignupForm` Customization

To customize the `SignupForm` component, you need to pass in the `additionalFields` prop. It can be either a list of extra fields or a render function.

<Tabs groupId="js-ts">
<TabItem value="js" label="JavaScript">

```jsx title="client/SignupPage.jsx"
import { SignupForm } from "@wasp/auth/forms/Signup";
import {
  FormError,
  FormInput,
  FormItemGroup,
  FormLabel,
} from "@wasp/auth/forms/internal/Form";

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

</TabItem>
<TabItem value="ts" label="TypeScript">

```tsx title="client/SignupPage.tsx"
import { SignupForm } from "@wasp/auth/forms/Signup";
import {
  FormError,
  FormInput,
  FormItemGroup,
  FormLabel,
} from "@wasp/auth/forms/internal/Form";

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
   (form: UseFormReturn, state: FormState) => React.ReactNode;
   ```

   - `form` <Required />

     - the `react-hook-form` object, read more about it in the [react-hook-form docs](https://react-hook-form.com/api/useform)
     - you need to use the `form.register` function to register your fields

   - `state` <Required />

     - the form state object which has the following properties:
       - `isLoading: boolean`
         - whether the form is currently submitting
