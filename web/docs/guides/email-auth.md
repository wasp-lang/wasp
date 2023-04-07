---
title: Email Authentication
---

# Email Authentication

## Overview

Wasp supports e-mail authentication out of the box. It provides a set of routes and email templates that you can use to implement email authentication in your app.

![Auth UI](/img/authui/all_screens.gif)

In this guide, we'll go through the easiest way to set up email authentication: using Wasp's Auth UI components.

## Outline of the guide

We'll need to do the following steps to set up email authentication:
- [ ] Set up email authentication in `main.wasp`
- [ ] Define the user entity
- [ ] Define the routes and pages
- [ ] Set up the email sender in `main.wasp` and `.env.server`
- [ ] Use Auth UI in our pages

Outline of the Wasp file we'll be working with:

```c title="main.wasp"
// Configuring e-mail authentication
app myApp { ... }

// Defining User entity
entity User { ... }

// Defining routes and pages
route SignupRoute { ... }
page SignupPage { ... }
// ...
```

### Email authentication in `main.wasp`

Let's first set up the email authentication by adding the following to our `main.wasp` file:

```c title="main.wasp"
app myApp {
  wasp: {
    version: "^0.10.0"
  },
  title: "My App",
  auth: {
    // 1. Specify the user entity
    userEntity: User,
    methods: {
      // 2. Enable email authentication
      email: {
        // 3. Specify the email from field
        fromField: {
          name: "My App Postman",
          email: "hello@itsme.com"
        },
        // 4. Specify the email verification and password reset options
        emailVerification: {
          clientRoute: EmailVerificationRoute,
          getEmailContentFn: import { getVerificationEmailContent } from "@server/auth/email.js",
          allowUnverifiedLogin: false,
        },
        passwordReset: {
          clientRoute: PasswordResetRoute,
          getEmailContentFn: import { getPasswordResetEmailContent } from "@server/auth/email.js",
        },
      },
    },
    onAuthFailedRedirectTo: "/login",
    onAuthSucceededRedirectTo: "/profile"
  },
}
```

### User entity

Then we'll define the `User` entity in our `main.wasp` file:

```c title="main.wasp" {4-8}
// 5. Define the user entity
entity User {=psl
    id                        Int           @id @default(autoincrement())
    email                     String?       @unique
    password                  String?
    isEmailVerified           Boolean       @default(false)
    emailVerificationSentAt   DateTime?
    passwordResetSentAt       DateTime?
    // Add your own fields below
    // ...
psl=}
```

### Routes and pages

Next, we need to define the routes and pages for the authentication pages. We'll show the React code later, but for now we'll just define the routes and pages.

We'll add the following to our `main.wasp` file:

```c title="main.wasp"
// 6. Define the routes
route SignupRoute { path: "/signup", to: SignupPage }
page SignupPage {
  component: import { Signup } from "@client/pages/auth/Signup.tsx"
}

route LoginRoute { path: "/login", to: LoginPage }
page LoginPage {
  component: import { Login } from "@client/pages/auth/Login.tsx"
}

route RequestPasswordResetRoute { path: "/request-password-reset", to: RequestPasswordResetPage }
page RequestPasswordResetPage {
  component: import { RequestPasswordReset } from "@client/pages/auth/RequestPasswordReset.tsx",
}

route PasswordResetRoute { path: "/password-reset", to: PasswordResetPage }
page PasswordResetPage {
  component: import { PasswordReset } from "@client/pages/auth/PasswordReset.tsx",
}

route EmailVerificationRoute { path: "/email-verification", to: EmailVerificationPage }
page EmailVerificationPage {
  component: import { EmailVerification } from "@client/pages/auth/EmailVerification.tsx",
}
```

### Email sender

We'll use SendGrid in this guide to send our e-mails. You can use any of the supported email providers. Read more about setting up the email sender in the [email sender setup guide](/docs/guides/sending-emails).

To set up SendGrid to send emails, we will add the following to our `main.wasp` file:

```c title="main.wasp"
app myApp {
  ...
  emailSender: {
    provider: SendGrid,
  }
}
```

... and add the following to our `.env.server` file:

```c title=".env.server"
SENDGRID_API_KEY=<your key>
```

## Using Auth UI

### Signup page

![Auth UI](/img/authui/signup.png)

We are using the `SignupForm` component from `@wasp/auth/forms/Signup` to render the signup form.

```tsx title="client/pages/auth/Signup.tsx"
import { Link } from 'react-router-dom'
import { SignupForm } from '@wasp/auth/forms/Signup'

export function Signup () {
  return (
    <div className="w-full h-full bg-white">
      <div className="min-w-full min-h-[75vh] flex items-center justify-center">
        <div className="w-full h-full max-w-sm p-5 bg-white">
          <div>
            <SignupForm />
            <br />
            <span className="text-sm font-medium text-gray-900">
              I already have an account (<Link to="/login">go to login</Link>).
            </span>
            <br />
          </div>
        </div>
      </div>
    </div>
  )
}
```

### Login page

![Auth UI](/img/authui/login.png)

We are using the `LoginForm` component from `@wasp/auth/forms/Login` to render the login form.

```tsx title="client/pages/auth/Login.tsx"
import { Link } from 'react-router-dom'
import { LoginForm } from '@wasp/auth/forms/Login'

export function Login() {
  return (
    <div className="w-full h-full bg-white">
      <div className="min-w-full min-h-[75vh] flex items-center justify-center">
        <div className="w-full h-full max-w-sm p-5 bg-white">
          <div>
            <LoginForm />
            <br />
            <span className="text-sm font-medium text-gray-900">
              Don't have an account yet? <Link to="/signup">go to signup</Link>.
            </span>
            <br />
            <span className="text-sm font-medium text-gray-900">
              Forgot your password?{' '}
              <Link to="/request-password-reset">reset it</Link>.
            </span>
          </div>
        </div>
      </div>
    </div>
  )
}
```

## Email verification setup

By default, Wasp requires the e-mail to be verified before allowing the user to log in. This is done by sending a verification email to the user's email address and requiring the user to click on a link in the email to verify their email address.

Our setup looks like this:

```c title="main.wasp"
emailVerification: {
    clientRoute: EmailVerificationRoute,
    getEmailContentFn: import { getVerificationEmailContent } from "@server/auth/email.js",
    allowUnverifiedLogin: false,
}
```

When the user receives an e-mail, they receive a link that goes to the client route specified in the `clientRoute` field. In our case, this is the `EmailVerificationRoute` route we defined in the `main.wasp` file.

```c title="main.wasp"
route EmailVerificationRoute { path: "/email-verification", to: EmailVerificationPage }
page EmailVerificationPage {
  component: import { EmailVerification } from "@client/pages/auth/EmailVerification.tsx",
}
```

### Email verification page

![Auth UI](/img/authui/email_verification.png)

This route goes to the `EmailVerification` page, which is defined in the `EmailVerification.tsx` file:

```jsx title="client/pages/auth/EmailVerification.tsx"
import { Link } from 'react-router-dom'
import { VerifyEmailForm } from '@wasp/auth/forms/VerifyEmail'

export function EmailVerification() {
  return (
    <div className="w-full h-full bg-white">
      <div className="min-w-full min-h-[75vh] flex items-center justify-center">
        <div className="w-full h-full max-w-sm p-5 bg-white">
          <div>
            <VerifyEmailForm />
            <br />
            <span className="text-sm font-medium text-gray-900">
              If everything is okay, <Link to="/login">go to login</Link>
            </span>
          </div>
        </div>
      </div>
    </div>
  )
}
```

You'll notice we are using the `VerifyEmailForm` component from the `@wasp/auth/forms/VerifyEmail` module. This will give a nice-looking form for the user to verify their e-mail. We are also using [Tailwind CSS](/docs/integrations/css-frameworks#tailwind) to style the page.

We will also override the default e-mail content. We are using the `getVerificationEmailContent` function from the `@server/auth/email.js` file to generate the email content.

```ts title="server/auth/email.ts"
import { GetVerificationEmailContentFn } from '@wasp/types'

export const getVerificationEmailContent: GetVerificationEmailContentFn = ({
  verificationLink,
}) => ({
  subject: 'Verify your email',
  text: `Click the link below to verify your email: ${verificationLink}`,
  html: `
        <p>Click the link below to verify your email</p>
        <a href="${verificationLink}">Verify email</a>
    `,
})
```

## Password reset setup

Users can request a password and then they'll receive an e-mail with a link to reset their password. 

Our setup in `main.wasp` looks like this:

```c title="main.wasp"
passwordReset: {
    clientRoute: PasswordResetRoute,
    getEmailContentFn: import { getPasswordResetEmailContent } from "@server/auth/email.js",
}
```

### Request password reset page

![Request password reset page](/img/authui/forgot_password_after.png)

They request their password to be reset by going to the `/request-password-reset` page. We'll add a link to this page in the login page.

That page will look like this:

```jsx title="client/pages/auth/RequestPasswordReset.tsx"
import { ForgotPasswordForm } from '@wasp/auth/forms/ForgotPassword'

export function RequestPasswordReset() {
  return (
    <div className="w-full h-full bg-white">
      <div className="min-w-full min-h-[75vh] flex items-center justify-center">
        <div className="w-full h-full max-w-sm p-5 bg-white">
          <div>
            <ForgotPasswordForm />
          </div>
        </div>
      </div>
    </div>
  )
}
```

We will also override the default e-mail content that's sent. We are using the `getVerificationEmailContent` function from the `@server/auth/email.js` file to generate the email content.

```ts title="server/auth/email.ts"
import { GetPasswordResetEmailContentFn } from '@wasp/types'

export const getPasswordResetEmailContent: GetPasswordResetEmailContentFn = ({
  passwordResetLink,
}) => ({
  subject: 'Password reset',
  text: `Click the link below to reset your password: ${passwordResetLink}`,
  html: `
        <p>Click the link below to reset your password</p>
        <a href="${passwordResetLink}">Reset password</a>
    `,
})
```

### Password reset page

![Request password reset page](/img/authui/reset_password_after.png)

When the user receives an e-mail, they receive a link that goes to the client route specified in the `clientRoute` field. In our case, this is the `PasswordResetRoute` route we defined in the `main.wasp` file.

```c title="main.wasp"
route PasswordResetRoute { path: "/password-reset", to: PasswordResetPage }
page PasswordResetPage {
  component: import { PasswordReset } from "@client/pages/auth/PasswordReset.tsx",
}
```

This route goes to the `PasswordResetPage` page, which is defined in the `PasswordReset.tsx` file. Users can enter their new password here:

```tsx title="client/pages/auth/PasswordReset.tsx"
import { Link } from 'react-router-dom'
import { ResetPasswordForm } from '@wasp/auth/forms/ResetPassword'

export function PasswordReset() {
  return (
    <div className="w-full h-full bg-white">
      <div className="min-w-full min-h-[75vh] flex items-center justify-center">
        <div className="w-full h-full max-w-sm p-5 bg-white">
          <div>
            <ResetPasswordForm />
            <br />
            <span className="text-sm font-medium text-gray-900">
              If everything is okay, <Link to="/login">go to login</Link>
            </span>
          </div>
        </div>
      </div>
    </div>
  )
}
```

### Logout action

To implement the logout action, you can use the `logout` function from the `@wasp/auth/logout` module. We can add it for example, to the `Navbar` component:

```jsx title="client/components/Navbar.tsx"
import logout from '@wasp/auth/logout';

export function Navbar() {
  return (
    <div>
      <button onClick={logout}>Logout</button>
    </div>
  )
}
```

### Getting the user in our client & server code

You read about our `useAuth` hook in [this section](/docs/language/features#accessing-the-currently-logged-in-user) of the docs.

In short, you can use the `useAuth` hook in your client code to get the currently logged-in user. If there is no user logged in, it will return `null`.

```jsx title="client/pages/Profile.tsx"
import useAuth from '@wasp/auth'

export function Profile() {
  const { data: user } = useAuth()

  if (!user) {
    return <div>You are not logged in!</div>
  }

  return (
    <div>
      Hello, {user.email}!
    </div>
  )
}
```

## Conclusion

And that's it! We now have a full authentication system in our app. We can register new users, login, logout, verify their e-mail, and reset their password.

We hope you enjoyed this guide and that you learned something new. If you have any questions, feel free to ask them on [our Discord server](https://discord.gg/rzdnErX).