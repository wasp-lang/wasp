---
title: Auth UI
---

import { EmailPill, UsernameAndPasswordPill, GithubPill, GooglePill, KeycloakPill, SlackPill, DiscordPill } from "./Pills";

To make using authentication in your app as easy as possible, Wasp provides auth actions, hooks, provider URLs, provider icons, and copy-paste Auth UI examples. This enables you to quickly get the login, signup, password reset, and email verification flows in your app while keeping the markup and styling in your codebase.

Below we cover the available component imports and copy-paste examples.

![Auth UI](/img/authui/all_screens.gif)

:::note

Remember that if you need a more custom approach, you can always [create your own UI](./overview.md#custom-auth-ui).

:::

## Overview

You can copy the drop-in examples below, or use the component imports if you are maintaining an app that already uses them.

Based on the authentication providers you enabled in your `main.wasp.ts` file, `wasp/client/auth` exposes the corresponding actions, hooks, provider URLs, and provider icons. For example, if you enabled e-mail authentication:

```ts title="main.wasp.ts"
import { app } from "@wasp.sh/spec"

export default app({
  name: "MyApp",
  //...
  auth: {
    methods: {
      // highlight-next-line
      email: {},
    },
    // ...
  },
  // ...
})
```

You can build a password form that looks like this:

![Auth UI](/img/authui/login.png)

And then if you enable Google and Github:

```ts title="main.wasp.ts"
import { app } from "@wasp.sh/spec"

export default app({
  name: "MyApp",
  //...
  auth: {
    methods: {
      email: {},
      // highlight-start
      google: {},
      gitHub: {},
      // highlight-end
    },
    // ...
  },
  // ...
})
```

You can include the Google and Github provider buttons to get this:

![Auth UI](/img/authui/multiple_providers.png)

Let's first go through the available component imports, then the copy-paste Auth UI examples.

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

```ts title="main.wasp.ts"
import { app, page, route } from "@wasp.sh/spec"
import { LoginPage } from "./src/LoginPage" with { type: "ref" }

export default app({
  // ...
  spec: [
    route("LoginRoute", "/login", page(LoginPage)),
  ],
})
```

<Tabs groupId="js-ts">
  <TabItem value="js" label="JavaScript">
    ```jsx title="src/LoginPage.jsx"
    import { LoginForm } from "wasp/client/auth"

    // Use it like this
    export function LoginPage() {
      return <LoginForm />
    }
    ```
  </TabItem>

  <TabItem value="ts" label="TypeScript">
    ```tsx title="src/LoginPage.tsx"
    import { LoginForm } from "wasp/client/auth"

    // Use it like this
    export function LoginPage() {
      return <LoginForm />
    }
    ```
  </TabItem>
</Tabs>

It will automatically show the correct authentication providers based on your `main.wasp.ts` file.

### Signup Form

Used with <UsernameAndPasswordPill />, <EmailPill />, <GithubPill />, <GooglePill />, <KeycloakPill />, <SlackPill /> and <DiscordPill /> authentication.

![Signup form](/img/authui/signup.png)

You can use the `SignupForm` component to build your signup page:

```ts title="main.wasp.ts"
import { app, page, route } from "@wasp.sh/spec"
import { SignupPage } from "./src/SignupPage" with { type: "ref" }

export default app({
  // ...
  spec: [
    route("SignupRoute", "/signup", page(SignupPage)),
  ],
})
```

<Tabs groupId="js-ts">
  <TabItem value="js" label="JavaScript">
    ```jsx title="src/SignupPage.jsx"
    import { SignupForm } from "wasp/client/auth"

    // Use it like this
    export function SignupPage() {
      return <SignupForm />
    }
    ```
  </TabItem>

  <TabItem value="ts" label="TypeScript">
    ```tsx title="src/SignupPage.tsx"
    import { SignupForm } from "wasp/client/auth"

    // Use it like this
    export function SignupPage() {
      return <SignupForm />
    }
    ```
  </TabItem>
</Tabs>

It will automatically show the correct authentication providers based on your `main.wasp.ts` file.

Read more about customizing the signup process like adding additional fields or extra UI in the [Auth Overview](../auth/overview#customizing-the-signup-process) section.

### Forgot Password Form

Used with <EmailPill /> authentication.

If users forget their password, they can use this form to reset it.

![Forgot password form](/img/authui/forgot_password.png)

You can use the `ForgotPasswordForm` component to build your own forgot password page:

```ts title="main.wasp.ts"
import { app, page, route } from "@wasp.sh/spec"
import { ForgotPasswordPage } from "./src/ForgotPasswordPage" with { type: "ref" }

export default app({
  // ...
  spec: [
    route(
      "RequestPasswordResetRoute",
      "/request-password-reset",
      page(ForgotPasswordPage)
    ),
  ],
})
```

<Tabs groupId="js-ts">
  <TabItem value="js" label="JavaScript">
    ```jsx title="src/ForgotPasswordPage.jsx"
    import { ForgotPasswordForm } from "wasp/client/auth"

    // Use it like this
    export function ForgotPasswordPage() {
      return <ForgotPasswordForm />
    }
    ```
  </TabItem>

  <TabItem value="ts" label="TypeScript">
    ```tsx title="src/ForgotPasswordPage.tsx"
    import { ForgotPasswordForm } from "wasp/client/auth"

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

```ts title="main.wasp.ts"
import { app, page, route } from "@wasp.sh/spec"
import { ResetPasswordPage } from "./src/ResetPasswordPage" with { type: "ref" }

export default app({
  // ...
  spec: [
    route("PasswordResetRoute", "/password-reset", page(ResetPasswordPage)),
  ],
})
```

<Tabs groupId="js-ts">
  <TabItem value="js" label="JavaScript">
    ```jsx title="src/ResetPasswordPage.jsx"
    import { ResetPasswordForm } from "wasp/client/auth"

    // Use it like this
    export function ResetPasswordPage() {
      return <ResetPasswordForm />
    }
    ```
  </TabItem>

  <TabItem value="ts" label="TypeScript">
    ```tsx title="src/ResetPasswordPage.tsx"
    import { ResetPasswordForm } from "wasp/client/auth"

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

```ts title="main.wasp.ts"
import { app, page, route } from "@wasp.sh/spec"
import { VerifyEmailPage } from "./src/VerifyEmailPage" with { type: "ref" }

export default app({
  // ...
  spec: [
    route("EmailVerificationRoute", "/email-verification", page(VerifyEmailPage)),
  ],
})
```

<Tabs groupId="js-ts">
  <TabItem value="js" label="JavaScript">
    ```jsx title="src/VerifyEmailPage.jsx"
    import { VerifyEmailForm } from "wasp/client/auth"

    // Use it like this
    export function VerifyEmailPage() {
      return <VerifyEmailForm />
    }
    ```
  </TabItem>

  <TabItem value="ts" label="TypeScript">
    ```tsx title="src/VerifyEmailPage.tsx"
    import { VerifyEmailForm } from "wasp/client/auth"

    // Use it like this
    export function VerifyEmailPage() {
      return <VerifyEmailForm />
    }
    ```
  </TabItem>
</Tabs>

## Drop-In Custom Auth UI

Copy one of these examples into your app to get Auth UI with explicit markup and styling you own. The hooks from `wasp/client/auth` own form fields, loading state, field errors, global errors, success messages, and disabled state. No Auth UI CSS is imported from Wasp, so use the Tailwind version or copy the plain CSS version into your app.

The examples below assume e-mail auth plus Google and GitHub OAuth. Remove OAuth providers you have not enabled, or add other enabled providers by importing their sign-in URL and icon primitive from `wasp/client/auth` (`DiscordIcon`, `GoogleIcon`, `GitHubIcon`, `KeycloakIcon`, `MicrosoftIcon`, or `SlackIcon`). For username and password auth, change `identityField` to `"username"` and call the username auth `login` and `signup` actions instead.

<Tabs groupId="drop-in-auth-ui">
  <TabItem value="tailwind" label="Tailwind">
    ```tsx title="src/auth/DropInAuthForms.tsx" auto-js
    import {
      githubSignInUrl,
      googleSignInUrl,
      login,
      requestPasswordReset,
      resetPassword,
      signup,
      verifyEmail,
      GitHubIcon,
      GoogleIcon,
      useForgotPasswordForm,
      useLoginForm,
      useOAuthProviderActions,
      useResetPasswordForm,
      useSignupForm,
      useVerifyEmail,
      type ErrorMessage,
      type OAuthProvider,
    } from "wasp/client/auth"
    import type { ComponentPropsWithoutRef, ReactNode } from "react"
    import { useLocation, useNavigate } from "react-router"

    const oauthProviders: OAuthProvider[] = [
      { id: "google", label: "Google", href: googleSignInUrl },
      { id: "github", label: "GitHub", href: githubSignInUrl },
    ]

    const providerIcons = {
      google: GoogleIcon,
      github: GitHubIcon,
    }

    export function LoginPage() {
      const navigate = useNavigate()
      const form = useLoginForm({
        identityField: "email",
        submit: login,
        onSuccess() {
          navigate("/")
        },
      })

      return (
        <AuthShell title="Log in">
          <AuthNotice
            errorMessage={form.errorMessage}
            successMessage={form.successMessage}
          />
          <OAuthButtons action="Log in" />
          <AuthDivider />
          <form className="mt-6 space-y-6" onSubmit={(event) => void form.submit(event)}>
            <Field
              {...form.getFieldProps("email")}
              label="E-mail"
              type="email"
              autoComplete="email"
              error={form.fieldErrors.email}
            />
            <Field
              {...form.getFieldProps("password")}
              label="Password"
              type="password"
              autoComplete="current-password"
              error={form.fieldErrors.password}
            />
            <SubmitButton isSubmitting={form.isSubmitting}>Log in</SubmitButton>
          </form>
          <p className="mt-6 text-center text-sm">
            Don't have an account? <a className="font-medium underline" href="/signup">Sign up</a>
          </p>
        </AuthShell>
      )
    }

    export function SignupPage() {
      const form = useSignupForm({
        identityField: "email",
        submit: signup,
        resetOnSuccess: true,
        successMessage:
          "You've signed up successfully! Check your email for the confirmation link.",
      })

      return (
        <AuthShell title="Sign up">
          <AuthNotice
            errorMessage={form.errorMessage}
            successMessage={form.successMessage}
          />
          <OAuthButtons action="Sign up" />
          <AuthDivider />
          <form className="mt-6 space-y-6" onSubmit={(event) => void form.submit(event)}>
            <Field
              {...form.getFieldProps("email")}
              label="E-mail"
              type="email"
              autoComplete="email"
              error={form.fieldErrors.email}
            />
            <Field
              {...form.getFieldProps("password")}
              label="Password"
              type="password"
              autoComplete="new-password"
              error={form.fieldErrors.password}
            />
            <SubmitButton isSubmitting={form.isSubmitting}>Sign up</SubmitButton>
          </form>
          <p className="mt-6 text-center text-sm">
            Already have an account? <a className="font-medium underline" href="/login">Log in</a>
          </p>
        </AuthShell>
      )
    }

    export function ForgotPasswordPage() {
      const form = useForgotPasswordForm({
        submit: requestPasswordReset,
      })

      return (
        <AuthShell title="Request password reset">
          <AuthNotice
            errorMessage={form.errorMessage}
            successMessage={form.successMessage}
          />
          <form className="mt-6 space-y-6" onSubmit={(event) => void form.submit(event)}>
            <Field
              {...form.getFieldProps("email")}
              label="E-mail"
              type="email"
              autoComplete="email"
              error={form.fieldErrors.email}
            />
            <SubmitButton isSubmitting={form.isSubmitting}>
              Send password reset email
            </SubmitButton>
          </form>
        </AuthShell>
      )
    }

    export function ResetPasswordPage() {
      const location = useLocation()
      const token = new URLSearchParams(location.search).get("token")
      const form = useResetPasswordForm({
        token,
        submit: ({ password, token: resetToken }) =>
          resetPassword({ password, token: resetToken }),
      })

      return (
        <AuthShell title="Reset password">
          <AuthNotice
            errorMessage={form.errorMessage}
            successMessage={form.successMessage}
          />
          <form className="mt-6 space-y-6" onSubmit={(event) => void form.submit(event)}>
            <Field
              {...form.getFieldProps("password")}
              label="New password"
              type="password"
              autoComplete="new-password"
              error={form.fieldErrors.password}
            />
            <Field
              {...form.getFieldProps("passwordConfirmation")}
              label="Confirm new password"
              type="password"
              autoComplete="new-password"
              error={form.fieldErrors.passwordConfirmation}
            />
            <SubmitButton isSubmitting={form.isSubmitting}>Reset password</SubmitButton>
          </form>
        </AuthShell>
      )
    }

    export function VerifyEmailPage() {
      const location = useLocation()
      const token = new URLSearchParams(location.search).get("token")
      const form = useVerifyEmail({
        token,
        verify: verifyEmail,
      })

      return (
        <AuthShell title="Verify email">
          <AuthNotice
            loadingMessage={form.isSubmitting ? "Verifying email..." : null}
            errorMessage={form.errorMessage}
            successMessage={form.successMessage}
          />
        </AuthShell>
      )
    }

    function AuthShell({ children, title }: { children: ReactNode; title: string }) {
      return (
        <main className="mx-auto flex min-h-screen w-full max-w-sm flex-col px-4 py-12">
          <h1 className="mt-6 text-3xl font-bold tracking-tight">{title}</h1>
          {children}
        </main>
      )
    }

    function OAuthButtons({ action }: { action: "Log in" | "Sign up" }) {
      const socialAuth = useOAuthProviderActions({
        providers: oauthProviders.map((provider) => ({
          ...provider,
          start: redirectToProvider,
        })),
      })

      return (
        <div className="mt-6">
          <p className="text-sm font-medium">{action} with</p>
          <AuthNotice errorMessage={socialAuth.errorMessage} />
          <div className="mt-2 grid grid-cols-2 gap-4">
            {socialAuth.providers.map((provider) => {
              const ProviderIcon = providerIcons[provider.id as keyof typeof providerIcons]
              return (
                <a
                  key={provider.id}
                  {...provider.getLinkProps()}
                  className="flex justify-center rounded-md border border-gray-300 bg-gray-100 px-3 py-2 text-inherit shadow-sm transition hover:bg-gray-200 aria-disabled:pointer-events-none aria-disabled:opacity-50"
                  aria-label={`${action} with ${provider.label}`}
                >
                  <ProviderIcon className="h-5 w-5" />
                </a>
              )
            })}
          </div>
        </div>
      )
    }

    function redirectToProvider(provider: OAuthProvider) {
      if (provider.href) {
        window.location.href = provider.href
      }
    }

    function AuthDivider() {
      return (
        <div className="relative mt-6">
          <div className="absolute inset-0 flex items-center" aria-hidden="true">
            <div className="w-full border-t border-gray-300" />
          </div>
          <div className="relative flex justify-center text-sm">
            <span className="bg-white px-2">Or continue with</span>
          </div>
        </div>
      )
    }

    function AuthNotice({
      errorMessage,
      loadingMessage,
      successMessage,
    }: {
      errorMessage?: ErrorMessage | null
      loadingMessage?: string | null
      successMessage?: string | null
    }) {
      if (errorMessage) {
        return (
          <div className="mt-4 rounded-md bg-red-100 px-3 py-2 text-slate-700" role="alert">
            {formatError(errorMessage)}
          </div>
        )
      }

      if (successMessage) {
        return (
          <div className="mt-4 rounded-md bg-green-100 px-3 py-2 text-slate-700" role="status">
            {successMessage}
          </div>
        )
      }

      if (loadingMessage) {
        return <div className="mt-4 rounded-md bg-gray-100 px-3 py-2">{loadingMessage}</div>
      }

      return null
    }

    type FieldProps = ComponentPropsWithoutRef<"input"> & {
      error?: string
      label: string
    }

    function Field({ error, label, ...inputProps }: FieldProps) {
      return (
        <label className="block">
          <span className="mb-2 block text-sm font-medium">{label}</span>
          <input
            {...inputProps}
            className="block w-full rounded-md border border-gray-300 bg-purple-50 px-3 py-1.5 text-sm leading-6 shadow-sm outline-none focus:border-gray-400 disabled:cursor-not-allowed disabled:border-gray-100 disabled:bg-gray-100 disabled:text-gray-500 disabled:opacity-50"
          />
          {error && <span className="mt-2 block text-sm font-medium text-red-600">{error}</span>}
        </label>
      )
    }

    function SubmitButton({
      children,
      isSubmitting,
    }: {
      children: ReactNode
      isSubmitting: boolean
    }) {
      return (
        <button
          className="flex w-full justify-center rounded-md border border-yellow-300 bg-yellow-300 px-3 py-2 text-sm font-semibold text-black shadow-sm transition hover:border-yellow-400 hover:bg-yellow-400 disabled:cursor-not-allowed disabled:border-gray-100 disabled:bg-gray-100 disabled:text-gray-500 disabled:opacity-50"
          type="submit"
          disabled={isSubmitting}
        >
          {isSubmitting ? "Loading..." : children}
        </button>
      )
    }

    function formatError(errorMessage: ErrorMessage) {
      return [errorMessage.title, errorMessage.description].filter(Boolean).join(": ")
    }
    ```
  </TabItem>

  <TabItem value="css" label="Plain CSS">
    ```tsx title="src/auth/DropInAuthForms.tsx" auto-js
    import {
      githubSignInUrl,
      googleSignInUrl,
      login,
      requestPasswordReset,
      resetPassword,
      signup,
      verifyEmail,
      GitHubIcon,
      GoogleIcon,
      useForgotPasswordForm,
      useLoginForm,
      useOAuthProviderActions,
      useResetPasswordForm,
      useSignupForm,
      useVerifyEmail,
      type ErrorMessage,
      type OAuthProvider,
    } from "wasp/client/auth"
    import type { ComponentPropsWithoutRef, ReactNode } from "react"
    import { useLocation, useNavigate } from "react-router"
    import "./DropInAuthForms.css"

    const oauthProviders: OAuthProvider[] = [
      { id: "google", label: "Google", href: googleSignInUrl },
      { id: "github", label: "GitHub", href: githubSignInUrl },
    ]

    const providerIcons = {
      google: GoogleIcon,
      github: GitHubIcon,
    }

    export function LoginPage() {
      const navigate = useNavigate()
      const form = useLoginForm({
        identityField: "email",
        submit: login,
        onSuccess() {
          navigate("/")
        },
      })

      return (
        <AuthShell title="Log in">
          <AuthNotice
            errorMessage={form.errorMessage}
            successMessage={form.successMessage}
          />
          <OAuthButtons action="Log in" />
          <AuthDivider />
          <form className="auth-form" onSubmit={(event) => void form.submit(event)}>
            <Field
              {...form.getFieldProps("email")}
              label="E-mail"
              type="email"
              autoComplete="email"
              error={form.fieldErrors.email}
            />
            <Field
              {...form.getFieldProps("password")}
              label="Password"
              type="password"
              autoComplete="current-password"
              error={form.fieldErrors.password}
            />
            <SubmitButton isSubmitting={form.isSubmitting}>Log in</SubmitButton>
          </form>
          <p className="auth-link-row">
            Don't have an account? <a href="/signup">Sign up</a>
          </p>
        </AuthShell>
      )
    }

    export function SignupPage() {
      const form = useSignupForm({
        identityField: "email",
        submit: signup,
        resetOnSuccess: true,
        successMessage:
          "You've signed up successfully! Check your email for the confirmation link.",
      })

      return (
        <AuthShell title="Sign up">
          <AuthNotice
            errorMessage={form.errorMessage}
            successMessage={form.successMessage}
          />
          <OAuthButtons action="Sign up" />
          <AuthDivider />
          <form className="auth-form" onSubmit={(event) => void form.submit(event)}>
            <Field
              {...form.getFieldProps("email")}
              label="E-mail"
              type="email"
              autoComplete="email"
              error={form.fieldErrors.email}
            />
            <Field
              {...form.getFieldProps("password")}
              label="Password"
              type="password"
              autoComplete="new-password"
              error={form.fieldErrors.password}
            />
            <SubmitButton isSubmitting={form.isSubmitting}>Sign up</SubmitButton>
          </form>
          <p className="auth-link-row">
            Already have an account? <a href="/login">Log in</a>
          </p>
        </AuthShell>
      )
    }

    export function ForgotPasswordPage() {
      const form = useForgotPasswordForm({
        submit: requestPasswordReset,
      })

      return (
        <AuthShell title="Request password reset">
          <AuthNotice
            errorMessage={form.errorMessage}
            successMessage={form.successMessage}
          />
          <form className="auth-form" onSubmit={(event) => void form.submit(event)}>
            <Field
              {...form.getFieldProps("email")}
              label="E-mail"
              type="email"
              autoComplete="email"
              error={form.fieldErrors.email}
            />
            <SubmitButton isSubmitting={form.isSubmitting}>
              Send password reset email
            </SubmitButton>
          </form>
        </AuthShell>
      )
    }

    export function ResetPasswordPage() {
      const location = useLocation()
      const token = new URLSearchParams(location.search).get("token")
      const form = useResetPasswordForm({
        token,
        submit: ({ password, token: resetToken }) =>
          resetPassword({ password, token: resetToken }),
      })

      return (
        <AuthShell title="Reset password">
          <AuthNotice
            errorMessage={form.errorMessage}
            successMessage={form.successMessage}
          />
          <form className="auth-form" onSubmit={(event) => void form.submit(event)}>
            <Field
              {...form.getFieldProps("password")}
              label="New password"
              type="password"
              autoComplete="new-password"
              error={form.fieldErrors.password}
            />
            <Field
              {...form.getFieldProps("passwordConfirmation")}
              label="Confirm new password"
              type="password"
              autoComplete="new-password"
              error={form.fieldErrors.passwordConfirmation}
            />
            <SubmitButton isSubmitting={form.isSubmitting}>Reset password</SubmitButton>
          </form>
        </AuthShell>
      )
    }

    export function VerifyEmailPage() {
      const location = useLocation()
      const token = new URLSearchParams(location.search).get("token")
      const form = useVerifyEmail({
        token,
        verify: verifyEmail,
      })

      return (
        <AuthShell title="Verify email">
          <AuthNotice
            loadingMessage={form.isSubmitting ? "Verifying email..." : null}
            errorMessage={form.errorMessage}
            successMessage={form.successMessage}
          />
        </AuthShell>
      )
    }

    function AuthShell({ children, title }: { children: ReactNode; title: string }) {
      return (
        <main className="auth-shell">
          <h1 className="auth-title">{title}</h1>
          {children}
        </main>
      )
    }

    function OAuthButtons({ action }: { action: "Log in" | "Sign up" }) {
      const socialAuth = useOAuthProviderActions({
        providers: oauthProviders.map((provider) => ({
          ...provider,
          start: redirectToProvider,
        })),
      })

      return (
        <div className="auth-social">
          <p className="auth-social-label">{action} with</p>
          <AuthNotice errorMessage={socialAuth.errorMessage} />
          <div className="auth-social-buttons">
            {socialAuth.providers.map((provider) => {
              const ProviderIcon = providerIcons[provider.id as keyof typeof providerIcons]
              return (
                <a
                  key={provider.id}
                  {...provider.getLinkProps()}
                  className="auth-social-button"
                  aria-label={`${action} with ${provider.label}`}
                >
                  <ProviderIcon className="auth-social-icon" />
                </a>
              )
            })}
          </div>
        </div>
      )
    }

    function redirectToProvider(provider: OAuthProvider) {
      if (provider.href) {
        window.location.href = provider.href
      }
    }

    function AuthDivider() {
      return (
        <div className="auth-divider">
          <div className="auth-divider-line" aria-hidden="true" />
          <div className="auth-divider-text">Or continue with</div>
        </div>
      )
    }

    function AuthNotice({
      errorMessage,
      loadingMessage,
      successMessage,
    }: {
      errorMessage?: ErrorMessage | null
      loadingMessage?: string | null
      successMessage?: string | null
    }) {
      if (errorMessage) {
        return <div className="auth-message auth-message-error" role="alert">{formatError(errorMessage)}</div>
      }

      if (successMessage) {
        return <div className="auth-message auth-message-success" role="status">{successMessage}</div>
      }

      if (loadingMessage) {
        return <div className="auth-message">{loadingMessage}</div>
      }

      return null
    }

    type FieldProps = ComponentPropsWithoutRef<"input"> & {
      error?: string
      label: string
    }

    function Field({ error, label, ...inputProps }: FieldProps) {
      return (
        <label className="auth-field">
          <span className="auth-label">{label}</span>
          <input {...inputProps} className="auth-input" />
          {error && <span className="auth-field-error">{error}</span>}
        </label>
      )
    }

    function SubmitButton({
      children,
      isSubmitting,
    }: {
      children: ReactNode
      isSubmitting: boolean
    }) {
      return (
        <button className="auth-submit" type="submit" disabled={isSubmitting}>
          {isSubmitting ? "Loading..." : children}
        </button>
      )
    }

    function formatError(errorMessage: ErrorMessage) {
      return [errorMessage.title, errorMessage.description].filter(Boolean).join(": ")
    }
    ```

    ```css title="src/auth/DropInAuthForms.css"
    .auth-shell {
      display: flex;
      flex-direction: column;
      min-height: 100vh;
      width: 100%;
      max-width: 24rem;
      margin: 0 auto;
      padding: 3rem 1rem;
    }

    .auth-title {
      margin-top: 1.5rem;
      font-size: 1.875rem;
      font-weight: 700;
      line-height: 2.25rem;
      letter-spacing: -0.025em;
    }

    .auth-form {
      display: grid;
      gap: 1.5rem;
      margin-top: 1.5rem;
    }

    .auth-field {
      display: block;
    }

    .auth-label,
    .auth-social-label {
      display: block;
      margin-bottom: 0.5rem;
      font-size: 1rem;
      font-weight: 500;
    }

    .auth-input {
      display: block;
      width: 100%;
      margin: 0;
      padding: 0.375rem 0.75rem;
      border: 1px solid #d1d5db;
      border-radius: 0.375rem;
      background-color: #f8f4ff;
      box-shadow: 0 1px 2px rgb(0 0 0 / 0.05);
      font-size: 1rem;
      line-height: 1.5rem;
    }

    .auth-input:focus {
      border-color: #a1a5ab;
      outline: none;
    }

    .auth-input:disabled,
    .auth-submit:disabled {
      cursor: not-allowed;
      border-color: #f0f0f0;
      background-color: #f0f0f0;
      color: #6b7280;
      opacity: 0.5;
    }

    .auth-field-error {
      display: block;
      margin-top: 0.5rem;
      color: #fa3838;
      font-size: 1rem;
      font-weight: 500;
    }

    .auth-submit {
      display: flex;
      justify-content: center;
      width: 100%;
      padding: 0.5rem 0.75rem;
      border: 1px solid #f5c842;
      border-radius: 0.375rem;
      background-color: #f5c842;
      color: black;
      box-shadow: 0 1px 2px rgb(0 0 0 / 0.05);
      cursor: pointer;
      font-size: 1rem;
      font-weight: 600;
      line-height: 1.25rem;
      transition: background-color 100ms, border-color 100ms;
    }

    .auth-submit:hover:not(:disabled) {
      border-color: #fccf49;
      background-color: #fccf49;
    }

    .auth-social {
      margin-top: 1.5rem;
    }

    .auth-social-buttons {
      display: grid;
      grid-template-columns: repeat(2, minmax(0, 1fr));
      gap: 1rem;
      margin-top: 0.5rem;
    }

    .auth-social-button {
      display: flex;
      justify-content: center;
      padding: 0.5rem 0.75rem;
      border: 1px solid #d1d5db;
      border-radius: 0.375rem;
      background-color: #f0f0f0;
      color: inherit;
      box-shadow: 0 1px 2px rgb(0 0 0 / 0.05);
      text-decoration: none;
      transition: background-color 100ms;
    }

    .auth-social-button:hover {
      background-color: gainsboro;
      color: inherit;
    }

    .auth-social-button[aria-disabled="true"] {
      pointer-events: none;
      opacity: 0.5;
    }

    .auth-social-icon {
      width: 1.25rem;
      height: 1.25rem;
    }

    .auth-divider {
      position: relative;
      margin-top: 1.5rem;
      text-align: center;
      font-size: 1rem;
    }

    .auth-divider-line {
      position: absolute;
      top: 50%;
      left: 0;
      width: 100%;
      border-top: 1px solid gainsboro;
    }

    .auth-divider-text {
      position: relative;
      display: inline-block;
      padding: 0 0.5rem;
      background-color: white;
    }

    .auth-message {
      margin-top: 1rem;
      padding: 0.5rem 0.75rem;
      border-radius: 0.375rem;
      background-color: #f0f0f0;
    }

    .auth-message-error {
      background-color: #fed7d7;
      color: #2d3748;
    }

    .auth-message-success {
      background-color: #c6f6d5;
      color: #2d3748;
    }

    .auth-link-row {
      margin-top: 1.5rem;
      text-align: center;
      font-size: 0.875rem;
    }

    .auth-link-row a {
      font-weight: 500;
      text-decoration: underline;
    }
    ```
  </TabItem>
</Tabs>

## Component Customization 💅🏻

You customize the component imports by passing props to them.

Props you can pass to all of the forms:

1. `appearance` - customize the form colors (via design tokens)
2. `logo` - path to your logo
3. `socialLayout` - layout of the social buttons, which can be `vertical` or `horizontal`

### 1. Customizing the Colors

We use CSS variables in our styling so you can customize the styles by overriding the default theme tokens.

:::info List of all available tokens

See the [list of all available tokens](https://github.com/wasp-lang/wasp/blob/release/waspc/data/Generator/templates/sdk/wasp/auth/forms/types.ts) which you can override.

:::

<Tabs groupId="js-ts">
  <TabItem value="js" label="JavaScript">
    ```js title="src/appearance.js"
    export const authAppearance = {
      colors: {
        brand: "#5969b8", // blue
        brandAccent: "#de5998", // pink
        submitButtonText: "white",
      },
    }
    ```

    ```jsx title="src/LoginPage.jsx"
    import { LoginForm } from "wasp/client/auth"
    import { authAppearance } from "./appearance"

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
    import type { CustomizationOptions } from "wasp/client/auth"

    export const authAppearance: CustomizationOptions["appearance"] = {
      colors: {
        brand: "#5969b8", // blue
        brandAccent: "#de5998", // pink
        submitButtonText: "white",
      },
    }
    ```

    ```tsx title="src/LoginPage.tsx"
    import { LoginForm } from "wasp/client/auth"
    import { authAppearance } from "./appearance"

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
    ```jsx title="src/LoginPage.jsx"
    import { LoginForm } from "wasp/client/auth"
    import Logo from "./logo.png"

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
    import { LoginForm } from "wasp/client/auth"
    import Logo from "./logo.png"

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
    ```jsx title="src/LoginPage.jsx"
    import { LoginForm } from "wasp/client/auth"

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
    import { LoginForm } from "wasp/client/auth"

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
    ```js title="src/appearance.js"
    export const appearance = {
      colors: {
        brand: "#5969b8", // blue
        brandAccent: "#de5998", // pink
        submitButtonText: "white",
      },
    }
    ```

    ```jsx title="src/LoginPage.jsx"
    import { LoginForm } from "wasp/client/auth"

    import { authAppearance } from "./appearance"
    import todoLogo from "./todoLogo.png"

    export function LoginPage() {
      return <LoginForm appearance={appearance} logo={todoLogo} />
    }
    ```
  </TabItem>

  <TabItem value="ts" label="TypeScript">
    ```ts title="src/appearance.ts"
    import type { CustomizationOptions } from "wasp/client/auth"

    export const appearance: CustomizationOptions["appearance"] = {
      colors: {
        brand: "#5969b8", // blue
        brandAccent: "#de5998", // pink
        submitButtonText: "white",
      },
    }
    ```

    ```tsx title="src/LoginPage.tsx"
    import { LoginForm } from "wasp/client/auth"

    import { authAppearance } from "./appearance"
    import todoLogo from "./todoLogo.png"

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
