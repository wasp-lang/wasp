---
title: Auth UI
---

import { EmailPill, UsernameAndPasswordPill, GithubPill, GooglePill, KeycloakPill, DiscordPill } from "./Pills";

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

Used with <UsernameAndPasswordPill />, <EmailPill />, <GithubPill />, <GooglePill />, <KeycloakPill />, and <DiscordPill /> authentication.

![Login form](/img/authui/login.png)

You can use the `LoginForm` component to build your login page:

<Tabs groupId="js-ts">
<TabItem value="js" label="JavaScript">

```wasp title="main.wasp"
// ...

route LoginRoute { path: "/login", to: LoginPage }
page LoginPage {
  component: import { LoginPage } from "@src/LoginPage.jsx"
}
```

```tsx title="src/LoginPage.jsx"
import { LoginForm } from 'wasp/client/auth'

// Use it like this
export function LoginPage() {
  return <LoginForm />
}
```

</TabItem>
<TabItem value="ts" label="TypeScript">

```wasp title="main.wasp"
// ...

route LoginRoute { path: "/login", to: LoginPage }
page LoginPage {
  component: import { LoginPage } from "@src/LoginPage.tsx"
}
```

```tsx title="src/LoginPage.tsx"
import { LoginForm } from 'wasp/client/auth'

// Use it like this
export function LoginPage() {
  return <LoginForm />
}
```

</TabItem>
</Tabs>

It will automatically show the correct authentication providers based on your `main.wasp` file.

### Signup Form

Used with <UsernameAndPasswordPill />, <EmailPill />, <GithubPill />, <GooglePill />, <KeycloakPill />, and <DiscordPill /> authentication.

![Signup form](/img/authui/signup.png)

You can use the `SignupForm` component to build your signup page:

<Tabs groupId="js-ts">
<TabItem value="js" label="JavaScript">

```wasp title="main.wasp"
// ...

route SignupRoute { path: "/signup", to: SignupPage }
page SignupPage {
  component: import { SignupPage } from "@src/SignupPage.jsx"
}
```

```tsx title="src/SignupPage.jsx"
import { SignupForm } from 'wasp/client/auth'

// Use it like this
export function SignupPage() {
  return <SignupForm />
}
```

</TabItem>
<TabItem value="ts" label="TypeScript">

```wasp title="main.wasp"
// ...

route SignupRoute { path: "/signup", to: SignupPage }
page SignupPage {
  component: import { SignupPage } from "@src/SignupPage.tsx"
}
```

```tsx title="src/SignupPage.tsx"
import { SignupForm } from 'wasp/client/auth'

// Use it like this
export function SignupPage() {
  return <SignupForm />
}
```

</TabItem>
</Tabs>

It will automatically show the correct authentication providers based on your `main.wasp` file.

Read more about customizing the signup process like adding additional fields or extra UI in the [Auth Overview](../auth/overview#customizing-the-signup-process) section.

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
  component: import { ForgotPasswordPage } from "@src/ForgotPasswordPage.jsx"
}
```

```tsx title="src/ForgotPasswordPage.jsx"
import { ForgotPasswordForm } from 'wasp/client/auth'

// Use it like this
export function ForgotPasswordPage() {
  return <ForgotPasswordForm />
}
```

</TabItem>
<TabItem value="ts" label="TypeScript">

```wasp title="main.wasp"
// ...

route RequestPasswordResetRoute { path: "/request-password-reset", to: RequestPasswordResetPage }
page RequestPasswordResetPage {
  component: import { ForgotPasswordPage } from "@src/ForgotPasswordPage.tsx"
}
```

```tsx title="src/ForgotPasswordPage.tsx"
import { ForgotPasswordForm } from 'wasp/client/auth'

// Use it like this
export function ForgotPasswordPage() {
  return <ForgotPasswordForm />
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
  component: import { ResetPasswordPage } from "@src/ResetPasswordPage.jsx"
}
```

```tsx title="src/ResetPasswordPage.jsx"
import { ResetPasswordForm } from 'wasp/client/auth'

// Use it like this
export function ResetPasswordPage() {
  return <ResetPasswordForm />
}
```

</TabItem>
<TabItem value="ts" label="TypeScript">

```wasp title="main.wasp"
// ...

route PasswordResetRoute { path: "/password-reset", to: PasswordResetPage }
page PasswordResetPage {
  component: import { ResetPasswordPage } from "@src/ResetPasswordPage.tsx"
}
```

```tsx title="src/ResetPasswordPage.tsx"
import { ResetPasswordForm } from 'wasp/client/auth'

// Use it like this
export function ResetPasswordPage() {
  return <ResetPasswordForm />
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
  component: import { VerifyEmailPage } from "@src/VerifyEmailPage.jsx"
}
```

```tsx title="src/VerifyEmailPage.jsx"
import { VerifyEmailForm } from 'wasp/client/auth'

// Use it like this
export function VerifyEmailPage() {
  return <VerifyEmailForm />
}
```

</TabItem>
<TabItem value="ts" label="TypeScript">

```wasp title="main.wasp"
// ...

route EmailVerificationRoute { path: "/email-verification", to: EmailVerificationPage }
page EmailVerificationPage {
  component: import { VerifyEmailPage } from "@src/VerifyEmailPage.tsx"
}
```

```tsx title="src/VerifyEmailPage.tsx"
import { VerifyEmailForm } from 'wasp/client/auth'

// Use it like this
export function VerifyEmailPage() {
  return <VerifyEmailForm />
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

```js title="src/appearance.js"
export const authAppearance = {
  colors: {
    brand: '#5969b8', // blue
    brandAccent: '#de5998', // pink
    submitButtonText: 'white',
  },
}
```

```jsx title="src/LoginPage.jsx"
import { LoginForm } from 'wasp/client/auth'
import { authAppearance } from './appearance'

export function LoginPage() {
  return (
    <LoginForm
      // Pass the appearance object to the form
      appearance={authAppearance}
    />
  )
}
```

</TabItem>
<TabItem value="ts" label="TypeScript">

```ts title="src/appearance.ts"
import type { CustomizationOptions } from 'wasp/client/auth'

export const authAppearance: CustomizationOptions['appearance'] = {
  colors: {
    brand: '#5969b8', // blue
    brandAccent: '#de5998', // pink
    submitButtonText: 'white',
  },
}
```

```tsx title="src/LoginPage.tsx"
import { LoginForm } from 'wasp/client/auth'
import { authAppearance } from './appearance'

export function LoginPage() {
  return (
    <LoginForm
      // Pass the appearance object to the form
      appearance={authAppearance}
    />
  )
}
```

</TabItem>
</Tabs>

We recommend defining your appearance in a separate file and importing it into your components.

### 2. Using Your Logo

You can add your logo to the Auth UI by passing the `logo` prop to any of the components.

<Tabs groupId="js-ts">
<TabItem value="js" label="JavaScript">

```tsx title="src/LoginPage.jsx"
import { LoginForm } from 'wasp/client/auth'
import Logo from './logo.png'

export function LoginPage() {
  return (
    <LoginForm
      // Pass in the path to your logo
      logo={Logo}
    />
  )
}
```

</TabItem>
<TabItem value="ts" label="TypeScript">

```tsx title="src/LoginPage.tsx"
import { LoginForm } from 'wasp/client/auth'
import Logo from './logo.png'

export function LoginPage() {
  return (
    <LoginForm
      // Pass in the path to your logo
      logo={Logo}
    />
  )
}
```

</TabItem>
</Tabs>

### 3. Social Buttons Layout

You can change the layout of the social buttons by passing the `socialLayout` prop to any of the components. It can be either `vertical` or `horizontal` (default).

If we pass in `vertical`:

<Tabs groupId="js-ts">
<TabItem value="js" label="JavaScript">

```tsx title="src/LoginPage.jsx"
import { LoginForm } from 'wasp/client/auth'

export function LoginPage() {
  return (
    <LoginForm
      // Pass in the socialLayout prop
      socialLayout="vertical"
    />
  )
}
```

</TabItem>
<TabItem value="ts" label="TypeScript">

```tsx title="src/LoginPage.tsx"
import { LoginForm } from 'wasp/client/auth'

export function LoginPage() {
  return (
    <LoginForm
      // Pass in the socialLayout prop
      socialLayout="vertical"
    />
  )
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

```ts title="src/appearance.js"
export const appearance = {
  colors: {
    brand: '#5969b8', // blue
    brandAccent: '#de5998', // pink
    submitButtonText: 'white',
  },
}
```

```tsx title="src/LoginPage.jsx"
import { LoginForm } from 'wasp/client/auth'

import { authAppearance } from './appearance'
import todoLogo from './todoLogo.png'

export function LoginPage() {
  return <LoginForm appearance={appearance} logo={todoLogo} />
}
```

</TabItem>
<TabItem value="ts" label="TypeScript">

```ts title="src/appearance.ts"
import type { CustomizationOptions } from 'wasp/client/auth'

export const appearance: CustomizationOptions['appearance'] = {
  colors: {
    brand: '#5969b8', // blue
    brandAccent: '#de5998', // pink
    submitButtonText: 'white',
  },
}
```

```tsx title="src/LoginPage.tsx"
import { LoginForm } from 'wasp/client/auth'

import { authAppearance } from './appearance'
import todoLogo from './todoLogo.png'

export function LoginPage() {
  return <LoginForm appearance={appearance} logo={todoLogo} />
}
```

</TabItem>
</Tabs>

We get a form looking like this:

<div style={{ textAlign: 'center' }}>
  <img src="/img/authui/custom_login.gif" alt="Custom login form" />
</div>
