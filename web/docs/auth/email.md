---
title: Email
---

Wasp supports e-mail authentication out of the box, along with email verification and "forgot your password?" flows. It provides you with the server-side implementation and email templates for all of these flows.

![Auth UI](/img/authui/all_screens.gif)

## Outline

We'll need to take the following steps to set up email authentication:
1. Set up email authentication in `main.wasp`
1. Add the user entity
1. Add the routes and pages
1. Set up the email sender in `main.wasp` and `.env.server`
1. Use Auth UI components in our pages ✨

Structure of the `main.wasp` file we will end up with:

```wasp title="main.wasp"
// Configuring e-mail authentication
app myApp {
  auth: { ... }
}

// Defining User entity
entity User { ... }

// Defining routes and pages
route SignupRoute { ... }
page SignupPage { ... }
// ...
```

### 1. Enable email authentication in `main.wasp`

Let's start with adding the following to our `main.wasp` file:


<Tabs groupId="js-ts">
<TabItem value="js" label="JavaScript">

```wasp title="main.wasp"
app myApp {
  wasp: {
    version: "^0.11.0"
  },
  title: "My App",
  auth: {
    // 1. Specify the user entity (we'll define it next)
    userEntity: User,
    methods: {
      // 2. Enable email authentication
      email: {
        // 3. Specify the email from field
        fromField: {
          name: "My App Postman",
          email: "hello@itsme.com"
        },
        // 4. Specify the email verification and password reset options (we'll talk about them later)
        emailVerification: {
          clientRoute: EmailVerificationRoute,
          getEmailContentFn: import { getVerificationEmailContent } from "@server/auth/email.js",
        },
        passwordReset: {
          clientRoute: PasswordResetRoute,
          getEmailContentFn: import { getPasswordResetEmailContent } from "@server/auth/email.js",
        },
        allowUnverifiedLogin: false,
      },
    },
    onAuthFailedRedirectTo: "/login",
    onAuthSucceededRedirectTo: "/"
  },
}
```
</TabItem>
<TabItem value="ts" label="TypeScript">

```wasp title="main.wasp"
app myApp {
  wasp: {
    version: "^0.11.0"
  },
  title: "My App",
  auth: {
    // 1. Specify the user entity (we'll define it later)
    userEntity: User,
    methods: {
      // 2. Enable email authentication
      email: {
        // 3. Specify the email from field
        fromField: {
          name: "My App Postman",
          email: "hello@itsme.com"
        },
        // 4. Specify the email verification and password reset options (we'll talk about them later)
        emailVerification: {
          clientRoute: EmailVerificationRoute,
          getEmailContentFn: import { getVerificationEmailContent } from "@server/auth/email.js",
        },
        passwordReset: {
          clientRoute: PasswordResetRoute,
          getEmailContentFn: import { getPasswordResetEmailContent } from "@server/auth/email.js",
        },
        allowUnverifiedLogin: false,
      },
    },
    onAuthFailedRedirectTo: "/login",
    onAuthSucceededRedirectTo: "/"
  },
}
```
</TabItem>
</Tabs>


TOOD: explanation of the fields:
- `userEntity`
- `email.fromField`
- `email.emailVerification`
- `email.passwordReset`
- `email.allowUnverifiedLogin`

### 2. Add the user entity

Then we'll add the `User` entity in our `main.wasp` file:

<Tabs groupId="js-ts">
<TabItem value="js" label="JavaScript">

```wasp title="main.wasp" {4-8}
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
</TabItem>
<TabItem value="ts" label="TypeScript">

```wasp title="main.wasp" {4-8}
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
</TabItem>
</Tabs>

### 3. Add the routes and pages

Next, we need to define the routes and pages for the authentication pages. We'll show the React code later, but for now we'll just define the routes and pages.

Add the following to the `main.wasp` file:

<Tabs groupId="js-ts">
<TabItem value="js" label="JavaScript">

```wasp title="main.wasp"
// ...

// 6. Define the routes
route LoginRoute { path: "/login", to: LoginPage }
page LoginPage {
  component: import { Login } from "@client/pages/auth.jsx"
}

route SignupRoute { path: "/signup", to: SignupPage }
page SignupPage {
  component: import { Signup } from "@client/pages/auth.jsx"
}

route RequestPasswordResetRoute { path: "/request-password-reset", to: RequestPasswordResetPage }
page RequestPasswordResetPage {
  component: import { RequestPasswordReset } from "@client/pages/auth.jsx",
}

route PasswordResetRoute { path: "/password-reset", to: PasswordResetPage }
page PasswordResetPage {
  component: import { PasswordReset } from "@client/pages/auth.jsx",
}

route EmailVerificationRoute { path: "/email-verification", to: EmailVerificationPage }
page EmailVerificationPage {
  component: import { EmailVerification } from "@client/pages/auth.jsx",
}
```
</TabItem>
<TabItem value="ts" label="TypeScript">

```wasp title="main.wasp"
// ...

// 6. Define the routes
route LoginRoute { path: "/login", to: LoginPage }
page LoginPage {
  component: import { Login } from "@client/pages/auth.tsx"
}

route SignupRoute { path: "/signup", to: SignupPage }
page SignupPage {
  component: import { Signup } from "@client/pages/auth.tsx"
}

route RequestPasswordResetRoute { path: "/request-password-reset", to: RequestPasswordResetPage }
page RequestPasswordResetPage {
  component: import { RequestPasswordReset } from "@client/pages/auth.tsx",
}

route PasswordResetRoute { path: "/password-reset", to: PasswordResetPage }
page PasswordResetPage {
  component: import { PasswordReset } from "@client/pages/auth.tsx",
}

route EmailVerificationRoute { path: "/email-verification", to: EmailVerificationPage }
page EmailVerificationPage {
  component: import { EmailVerification } from "@client/pages/auth.tsx",
}
```
</TabItem>
</Tabs>

### 4. Set up an email sender

We'll use SendGrid in this guide to send our e-mails. You can use any of the supported email providers. Read more about setting up the email sender in the [email sender docs](/docs/advanced/email).

To set up SendGrid to send emails, we will add the following to our `main.wasp` file:

<Tabs groupId="js-ts">
<TabItem value="js" label="JavaScript">

```wasp title="main.wasp"
app myApp {
  // ...
  // 7. Set up the email sender
  emailSender: {
    provider: SendGrid,
  }
}
```
</TabItem>
<TabItem value="ts" label="TypeScript">

```wasp title="main.wasp"
app myApp {
  // ...
  // 7. Set up the email sender
  emailSender: {
    provider: SendGrid,
  }
}
```
</TabItem>
</Tabs>

... and add the following to our `.env.server` file:

```c title=".env.server"
SENDGRID_API_KEY=<your key>
```

If you are not sure how to get a SendGrid API key, read more [here](/docs/advanced/email#getting-the-api-key).

### 5. Create the client pages

:::info
We are using [Tailwind CSS](https://tailwindcss.com/) to style the pages. Read more about how to add it [here](/docs/integrations/css-frameworks#tailwind).
:::

We'll import all of the Wasp-generated forms and export pages which will use them.

<Tabs groupId="js-ts">
<TabItem value="js" label="JavaScript">

```tsx title="client/pages/auth.jsx"
import { LoginForm } from "@wasp/auth/forms/Login";
import { SignupForm } from "@wasp/auth/forms/Signup";
import { VerifyEmailForm } from "@wasp/auth/forms/VerifyEmail";
import { ForgotPasswordForm } from "@wasp/auth/forms/ForgotPassword";
import { ResetPasswordForm } from "@wasp/auth/forms/ResetPassword";
import { Link } from "react-router-dom";

export function Login() {
  return (
    <Layout>
      <LoginForm />
      <br />
      <span className="text-sm font-medium text-gray-900">
        Don't have an account yet? <Link to="/signup">go to signup</Link>.
      </span>
      <br />
      <span className="text-sm font-medium text-gray-900">
        Forgot your password? <Link to="/request-password-reset">reset it</Link>
        .
      </span>
    </Layout>
  );
}

export function Signup() {
  return (
    <Layout>
      <SignupForm />
      <br />
      <span className="text-sm font-medium text-gray-900">
        I already have an account (<Link to="/login">go to login</Link>).
      </span>
    </Layout>
  );
}

export function EmailVerification() {
  return (
    <Layout>
      <VerifyEmailForm />
      <br />
      <span className="text-sm font-medium text-gray-900">
        If everything is okay, <Link to="/login">go to login</Link>
      </span>
    </Layout>
  );
}

export function RequestPasswordReset() {
  return (
    <Layout>
      <ForgotPasswordForm />
    </Layout>
  );
}

export function PasswordReset() {
  return (
    <Layout>
      <ResetPasswordForm />
      <br />
      <span className="text-sm font-medium text-gray-900">
        If everything is okay, <Link to="/login">go to login</Link>
      </span>
    </Layout>
  );
}

// A layout component to center the content
export function Layout({ children }) {
  return (
    <div className="w-full h-full bg-white">
      <div className="min-w-full min-h-[75vh] flex items-center justify-center">
        <div className="w-full h-full max-w-sm p-5 bg-white">
          <div>{children}</div>
        </div>
      </div>
    </div>
  );
}
```
</TabItem>
<TabItem value="ts" label="TypeScript">

```tsx title="client/pages/auth.tsx"
import { LoginForm } from "@wasp/auth/forms/Login";
import { SignupForm } from "@wasp/auth/forms/Signup";
import { VerifyEmailForm } from "@wasp/auth/forms/VerifyEmail";
import { ForgotPasswordForm } from "@wasp/auth/forms/ForgotPassword";
import { ResetPasswordForm } from "@wasp/auth/forms/ResetPassword";
import { Link } from "react-router-dom";

export function Login() {
  return (
    <Layout>
      <LoginForm />
      <br />
      <span className="text-sm font-medium text-gray-900">
        Don't have an account yet? <Link to="/signup">go to signup</Link>.
      </span>
      <br />
      <span className="text-sm font-medium text-gray-900">
        Forgot your password? <Link to="/request-password-reset">reset it</Link>
        .
      </span>
    </Layout>
  );
}

export function Signup() {
  return (
    <Layout>
      <SignupForm />
      <br />
      <span className="text-sm font-medium text-gray-900">
        I already have an account (<Link to="/login">go to login</Link>).
      </span>
    </Layout>
  );
}

export function EmailVerification() {
  return (
    <Layout>
      <VerifyEmailForm />
      <br />
      <span className="text-sm font-medium text-gray-900">
        If everything is okay, <Link to="/login">go to login</Link>
      </span>
    </Layout>
  );
}

export function RequestPasswordReset() {
  return (
    <Layout>
      <ForgotPasswordForm />
    </Layout>
  );
}

export function PasswordReset() {
  return (
    <Layout>
      <ResetPasswordForm />
      <br />
      <span className="text-sm font-medium text-gray-900">
        If everything is okay, <Link to="/login">go to login</Link>
      </span>
    </Layout>
  );
}

// A layout component to center the content
export function Layout({ children }: { children: React.ReactNode }) {
  return (
    <div className="w-full h-full bg-white">
      <div className="min-w-full min-h-[75vh] flex items-center justify-center">
        <div className="w-full h-full max-w-sm p-5 bg-white">
          <div>{children}</div>
        </div>
      </div>
    </div>
  );
}
```
</TabItem>
</Tabs>

## Login and signup setup

### Login page

We defined our login page in the `auth.{jsx,tsx}` file above. Let's talk about some of the behavior you get out of the box.

TODO: write about the rate limiting and fake work security

![Auth UI](/img/authui/login.png)

### Signup page

We defined our signup page in the `auth.{jsx,tsx}` file. There are some default validation rules for the user passwords.

There is also a rate limiter for the signup requests. It is set to 1 request per minute.

TODO: write about password validation rules

![Auth UI](/img/authui/signup.png)


## Email verification setup

By default, Wasp requires the e-mail to be verified before allowing the user to log in. This is done by sending a verification email to the user's email address and requiring the user to click on a link in the email to verify their email address.

Our setup looks like this:

<Tabs groupId="js-ts">
<TabItem value="js" label="JavaScript">

```wasp title="main.wasp"
// ...

emailVerification: {
    clientRoute: EmailVerificationRoute,
    getEmailContentFn: import { getVerificationEmailContent } from "@server/auth/email.js",
}
```
</TabItem>
<TabItem value="ts" label="TypeScript">

```wasp title="main.wasp"
// ...

emailVerification: {
    clientRoute: EmailVerificationRoute,
    getEmailContentFn: import { getVerificationEmailContent } from "@server/auth/email.js",
}
```
</TabItem>
</Tabs>

When the user receives an e-mail, they receive a link that goes to the client route specified in the `clientRoute` field. In our case, this is the `EmailVerificationRoute` route we defined in the `main.wasp` file.

<Tabs groupId="js-ts">
<TabItem value="js" label="JavaScript">

```wasp title="main.wasp"
route EmailVerificationRoute { path: "/email-verification", to: EmailVerificationPage }
page EmailVerificationPage {
  component: import { EmailVerification } from "@client/pages/auth.jsx",
}
```
</TabItem>
<TabItem value="ts" label="TypeScript">

```wasp title="main.wasp"
route EmailVerificationRoute { path: "/email-verification", to: EmailVerificationPage }
page EmailVerificationPage {
  component: import { EmailVerification } from "@client/pages/auth.tsx",
}
```
</TabItem>
</Tabs>

### Email verification page

We defined our email verification page in the `auth.{jsx,tsx}` file.

![Auth UI](/img/authui/email_verification.png)

We will also override the default e-mail content. We are using the `getVerificationEmailContent` function from the `@server/auth/email.js` file to generate the email content.

<Tabs groupId="js-ts">
<TabItem value="js" label="JavaScript">

```ts title="server/auth/email.js"
export const getVerificationEmailContent = ({
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
</TabItem>
<TabItem value="ts" label="TypeScript">

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
</TabItem>
</Tabs>

## Password reset setup

Users can request a password and then they'll receive an e-mail with a link to reset their password. 

Our setup in `main.wasp` looks like this:

<Tabs groupId="js-ts">
<TabItem value="js" label="JavaScript">

```wasp title="main.wasp"
// ...

passwordReset: {
    clientRoute: PasswordResetRoute,
    getEmailContentFn: import { getPasswordResetEmailContent } from "@server/auth/email.js",
}
```
</TabItem>
<TabItem value="ts" label="TypeScript">

```wasp title="main.wasp"
// ...

passwordReset: {
    clientRoute: PasswordResetRoute,
    getEmailContentFn: import { getPasswordResetEmailContent } from "@server/auth/email.js",
}
```
</TabItem>
</Tabs>

### Request password reset page

We defined our request password reset page in the `auth.{jsx,tsx}` file.

![Request password reset page](/img/authui/forgot_password_after.png)

Users request their password to be reset by going to the `/request-password-reset` page. We added a link to this page in the login page.

We will also override the default e-mail content that's sent. We are using the `getPasswordResetEmailContent` function from the `@server/auth/email.js` file to generate the email content.

<Tabs groupId="js-ts">
<TabItem value="js" label="JavaScript">

```ts title="server/auth/email.js"
export const getPasswordResetEmailContent = ({
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
</TabItem>
<TabItem value="ts" label="TypeScript">

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
</TabItem>
</Tabs>

### Password reset page

We defined our password reset page in the `auth.{jsx,tsx}` file.

![Request password reset page](/img/authui/reset_password_after.png)

When the user receives an e-mail, they receive a link that goes to the client route specified in the `clientRoute` field. In our case, this is the `PasswordResetRoute` route we defined in the `main.wasp` file.

```wasp title="main.wasp"
route PasswordResetRoute { path: "/password-reset", to: PasswordResetPage }
page PasswordResetPage {
  component: import { PasswordReset } from "@client/pages/auth/PasswordReset.tsx",
}
```

This route goes to the `PasswordResetPage` page, which is defined in the `auth.{jsx,tsx}` file. Users can enter their new password there.

### Logout action

TODO: decide if we want to talk about it here, or reference the logout docs

### Getting the user in our client & server code

TODO: decide if we want to talk about it here, or reference the docs

## Conclusion

And that's it! 🎉 We can register new users, login, logout, verify their e-mail, and reset their password.

If you have any questions, feel free to ask them on [our Discord server](https://discord.gg/rzdnErX).