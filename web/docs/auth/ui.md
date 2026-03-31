---
title: Auth UI
---

import { EmailPill, UsernameAndPasswordPill, GithubPill, GooglePill, KeycloakPill, SlackPill, DiscordPill } from "./Pills";

To make using authentication in your app as easy as possible, Wasp generates the server-side code but also the client-side UI for you. It enables you to quickly get the login, signup, password reset and email verification flows in your app.

Below we cover all of the available UI components and how to use them.

![Auth UI](/img/authui/all_screens.gif)

:::note

Remember that if you need a more custom approach, you can always [create your own UI](./overview.md#custom-auth-ui).

:::

## Overview

After Wasp generates the UI components for your auth, you can use it as is, or customize it to your liking.

Based on the authentication providers you enabled in your `main.wasp` file, the Auth UI will show the corresponding UI (form and buttons). For example, if you enabled e-mail authentication:

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

You'll get the following UI:

![Auth UI](/img/authui/login.png)

And then if you enable Google and Github:

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

Used with <UsernameAndPasswordPill />, <EmailPill />, <GithubPill />, <GooglePill />, <KeycloakPill />, <SlackPill /> and <DiscordPill /> authentication.

![Login form](/img/authui/login.png)

You can use the `LoginForm` component to build your login page:

```wasp title="main.wasp"
    // ...

    route LoginRoute { path: "/login", to: LoginPage }
    page LoginPage {
      component: import { LoginPage } from "@src/LoginPage"
    }
```

```tsx title="src/LoginPage.tsx" auto-js
    import { LoginForm } from 'wasp/client/auth'

    // Use it like this
    export function LoginPage() {
      return <LoginForm />
    }
```

It will automatically show the correct authentication providers based on your `main.wasp` file.

### Signup Form

Used with <UsernameAndPasswordPill />, <EmailPill />, <GithubPill />, <GooglePill />, <KeycloakPill />, <SlackPill /> and <DiscordPill /> authentication.

![Signup form](/img/authui/signup.png)

You can use the `SignupForm` component to build your signup page:

```wasp title="main.wasp"
    // ...

    route SignupRoute { path: "/signup", to: SignupPage }
    page SignupPage {
      component: import { SignupPage } from "@src/SignupPage"
    }
```

```tsx title="src/SignupPage.tsx" auto-js
    import { SignupForm } from 'wasp/client/auth'

    // Use it like this
    export function SignupPage() {
      return <SignupForm />
    }
```

It will automatically show the correct authentication providers based on your `main.wasp` file.

Read more about customizing the signup process like adding additional fields or extra UI in the [Auth Overview](../auth/overview#customizing-the-signup-process) section.

### Forgot Password Form

Used with <EmailPill /> authentication.

If users forget their password, they can use this form to reset it.

![Forgot password form](/img/authui/forgot_password.png)

You can use the `ForgotPasswordForm` component to build your own forgot password page:

```wasp title="main.wasp"
    // ...

    route RequestPasswordResetRoute { path: "/request-password-reset", to: RequestPasswordResetPage }
    page RequestPasswordResetPage {
      component: import { ForgotPasswordPage } from "@src/ForgotPasswordPage"
    }
```

```tsx title="src/ForgotPasswordPage.tsx" auto-js
    import { ForgotPasswordForm } from 'wasp/client/auth'

    // Use it like this
    export function ForgotPasswordPage() {
      return <ForgotPasswordForm />
    }
```

### Reset Password Form

Used with <EmailPill /> authentication.

After users click on the link in the email they receive after submitting the forgot password form, they will be redirected to this form where they can reset their password.

![Reset password form](/img/authui/reset_password.png)

You can use the `ResetPasswordForm` component to build your reset password page:

```wasp title="main.wasp"
    // ...

    route PasswordResetRoute { path: "/password-reset", to: PasswordResetPage }
    page PasswordResetPage {
      component: import { ResetPasswordPage } from "@src/ResetPasswordPage"
    }
```

```tsx title="src/ResetPasswordPage.tsx" auto-js
    import { ResetPasswordForm } from 'wasp/client/auth'

    // Use it like this
    export function ResetPasswordPage() {
      return <ResetPasswordForm />
    }
```

### Verify Email Form

Used with <EmailPill /> authentication.

After users sign up, they will receive an email with a link to this form where they can verify their email.

![Verify email form](/img/authui/email_verification.png)

You can use the `VerifyEmailForm` component to build your email verification page:

```wasp title="main.wasp"
    // ...

    route EmailVerificationRoute { path: "/email-verification", to: EmailVerificationPage }
    page EmailVerificationPage {
      component: import { VerifyEmailPage } from "@src/VerifyEmailPage"
    }
```

```tsx title="src/VerifyEmailPage.tsx" auto-js
    import { VerifyEmailForm } from 'wasp/client/auth'

    // Use it like this
    export function VerifyEmailPage() {
      return <VerifyEmailForm />
    }
```

## Customization 💅🏻

You customize all of the available forms by passing props to them.

Props you can pass to all of the forms:

1. `appearance` - customize the form colors (via design tokens)
2. `logo` - path to your logo
3. `socialLayout` - layout of the social buttons, which can be `vertical` or `horizontal`

### 1. Customizing the Colors

We use CSS variables in our styling so you can customize the styles by overriding the default theme tokens.

:::info List of all available tokens

See the [list of all available tokens](https://github.com/wasp-lang/wasp/blob/release/waspc/data/Generator/templates/sdk/wasp/auth/forms/types.ts) which you can override.

:::

```ts title="src/appearance.ts" auto-js
    import type { CustomizationOptions } from 'wasp/client/auth'

    export const authAppearance: CustomizationOptions['appearance'] = {
      colors: {
        brand: '#5969b8', // blue
        brandAccent: '#de5998', // pink
        submitButtonText: 'white',
      },
    }
```

```tsx title="src/LoginPage.tsx" auto-js
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

We recommend defining your appearance in a separate file and importing it into your components.

### 2. Using Your Logo

You can add your logo to the Auth UI by passing the `logo` prop to any of the components.

```tsx title="src/LoginPage.tsx" auto-js
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

### 3. Social Buttons Layout

You can change the layout of the social buttons by passing the `socialLayout` prop to any of the components. It can be either `vertical` or `horizontal` (default).

If we pass in `vertical`:

```tsx title="src/LoginPage.tsx" auto-js
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

We get this:

![Vertical social buttons](/img/authui/vertical_social_buttons.png)

### Let's Put Everything Together 🪄

If we provide the logo and custom colors:

```ts title="src/appearance.ts" auto-js
    import type { CustomizationOptions } from 'wasp/client/auth'

    export const appearance: CustomizationOptions['appearance'] = {
      colors: {
        brand: '#5969b8', // blue
        brandAccent: '#de5998', // pink
        submitButtonText: 'white',
      },
    }
```

```tsx title="src/LoginPage.tsx" auto-js
    import { LoginForm } from 'wasp/client/auth'

    import { authAppearance } from './appearance'
    import todoLogo from './todoLogo.png'

    export function LoginPage() {
      return <LoginForm appearance={appearance} logo={todoLogo} />
    }
```

We get a form looking like this:

<div style={{ textAlign: 'center' }}>
  <img src="/img/authui/custom_login.gif" alt="Custom login form" />
</div>
