---
title: Auth UI
---

import { EmailPill, UsernameAndPasswordPill, GithubPill, GooglePill, KeycloakPill, SlackPill, DiscordPill } from "./Pills";

To make using authentication in your app as easy as possible, Wasp provides auth actions, hooks, provider URLs, provider icons, generated provider metadata, and copy-paste Auth UI examples. This enables you to quickly get the login, signup, password reset, and email verification flows in your app while keeping the markup and styling in your codebase.

Below we cover drop-in Auth UI examples you can copy into your app.

![Auth UI](/img/authui/all_screens.gif)

:::note

Remember that if you need a more custom approach, you can always [create your own UI](./overview.md#custom-auth-ui).

:::

## Overview

You can copy the drop-in examples below into your app and keep customizing from there.

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

## Drop-In Custom Auth UI

Copy one of these examples into your app to get Auth UI with explicit markup and styling you own. The hooks from `wasp/client/auth` own form fields, loading state, field errors, global errors, success messages, and disabled state. The generated `enabledOAuthProviders` and `passwordAuthIdentityField` values keep the copied UI in sync with your `main.wasp.ts` auth methods.

The examples below assume no custom `userSignupFields`; if you configured custom signup fields, add them to `initialFields` and render matching inputs with `form.getFieldProps`.

Use the same routes that you configured in `main.wasp.ts` for login, signup, password reset, and e-mail verification. If your auth config uses a custom `onAuthSucceededRedirectTo`, set the local `onAuthSucceededRedirectTo` constant in the examples to the same path.

<Tabs groupId="drop-in-auth-ui">
  <TabItem value="tailwind" label="Tailwind">
    ```tsx title="src/auth/DropInAuthForms.tsx" auto-js
    import {
      enabledOAuthProviders,
      login,
      passwordAuthIdentityField,
      providerIconById,
      requestPasswordReset,
      resetPassword,
      signup,
      verifyEmail,
      useForgotPasswordForm,
      useLoginForm,
      useOAuthProviderActions,
      useResetPasswordForm,
      useSignupForm,
      useVerifyEmail,
      type ErrorMessage,
    } from "wasp/client/auth"
    import type { ComponentPropsWithoutRef, ReactNode } from "react"
    import { useLocation, useNavigate } from "react-router"

    const onAuthSucceededRedirectTo = "/"
    const hasOAuthProviders = enabledOAuthProviders.length > 0
    const identityField = passwordAuthIdentityField ?? "email"
    const identityLabel = identityField === "email" ? "E-mail" : "Username"
    const identityType = identityField === "email" ? "email" : "text"
    const identityAutoComplete = identityField === "email" ? "email" : "username"

    export function LoginPage() {
      const navigate = useNavigate()
      const form = useLoginForm({
        identityField,
        submit: login,
        onSuccess() {
          navigate(onAuthSucceededRedirectTo)
        },
      })

      return (
        <AuthShell title="Log in">
          <AuthNotice
            errorMessage={form.errorMessage}
            successMessage={form.successMessage}
          />
          {hasOAuthProviders && (
            <>
              <OAuthButtons action="Log in" />
              <AuthDivider />
            </>
          )}
          <form className="mt-6 space-y-6" onSubmit={(event) => void form.submit(event)}>
            <Field
              {...form.getFieldProps(identityField)}
              label={identityLabel}
              type={identityType}
              autoComplete={identityAutoComplete}
              error={form.fieldErrors[identityField]}
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
        identityField,
        async submit(fields) {
          await signup(fields)
        },
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
          {hasOAuthProviders && (
            <>
              <OAuthButtons action="Sign up" />
              <AuthDivider />
            </>
          )}
          <form className="mt-6 space-y-6" onSubmit={(event) => void form.submit(event)}>
            <Field
              {...form.getFieldProps(identityField)}
              label={identityLabel}
              type={identityType}
              autoComplete={identityAutoComplete}
              error={form.fieldErrors[identityField]}
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
        providers: enabledOAuthProviders,
      })

      return (
        <div className="mt-6">
          <p className="text-sm font-medium">{action} with</p>
          <AuthNotice errorMessage={socialAuth.errorMessage} />
          <div className="mt-2 grid grid-cols-2 gap-4">
            {socialAuth.providers.map((provider) => {
              const ProviderIcon = providerIconById[provider.id]
              if (!ProviderIcon) {
                return null
              }
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
      enabledOAuthProviders,
      login,
      passwordAuthIdentityField,
      providerIconById,
      requestPasswordReset,
      resetPassword,
      signup,
      verifyEmail,
      useForgotPasswordForm,
      useLoginForm,
      useOAuthProviderActions,
      useResetPasswordForm,
      useSignupForm,
      useVerifyEmail,
      type ErrorMessage,
    } from "wasp/client/auth"
    import type { ComponentPropsWithoutRef, ReactNode } from "react"
    import { useLocation, useNavigate } from "react-router"
    import "./DropInAuthForms.css"

    const onAuthSucceededRedirectTo = "/"
    const hasOAuthProviders = enabledOAuthProviders.length > 0
    const identityField = passwordAuthIdentityField ?? "email"
    const identityLabel = identityField === "email" ? "E-mail" : "Username"
    const identityType = identityField === "email" ? "email" : "text"
    const identityAutoComplete = identityField === "email" ? "email" : "username"

    export function LoginPage() {
      const navigate = useNavigate()
      const form = useLoginForm({
        identityField,
        submit: login,
        onSuccess() {
          navigate(onAuthSucceededRedirectTo)
        },
      })

      return (
        <AuthShell title="Log in">
          <AuthNotice
            errorMessage={form.errorMessage}
            successMessage={form.successMessage}
          />
          {hasOAuthProviders && (
            <>
              <OAuthButtons action="Log in" />
              <AuthDivider />
            </>
          )}
          <form className="auth-form" onSubmit={(event) => void form.submit(event)}>
            <Field
              {...form.getFieldProps(identityField)}
              label={identityLabel}
              type={identityType}
              autoComplete={identityAutoComplete}
              error={form.fieldErrors[identityField]}
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
        identityField,
        async submit(fields) {
          await signup(fields)
        },
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
          {hasOAuthProviders && (
            <>
              <OAuthButtons action="Sign up" />
              <AuthDivider />
            </>
          )}
          <form className="auth-form" onSubmit={(event) => void form.submit(event)}>
            <Field
              {...form.getFieldProps(identityField)}
              label={identityLabel}
              type={identityType}
              autoComplete={identityAutoComplete}
              error={form.fieldErrors[identityField]}
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
        providers: enabledOAuthProviders,
      })

      return (
        <div className="auth-social">
          <p className="auth-social-label">{action} with</p>
          <AuthNotice errorMessage={socialAuth.errorMessage} />
          <div className="auth-social-buttons">
            {socialAuth.providers.map((provider) => {
              const ProviderIcon = providerIconById[provider.id]
              if (!ProviderIcon) {
                return null
              }
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

## Customization

The copied examples are regular app code. To customize colors, spacing, logos, or social button layout, edit the Tailwind classes or CSS directly.

When you change auth methods in `main.wasp.ts`, Wasp regenerates the `wasp/client/auth` metadata used by the copied UI. For example, adding `google` or `gitHub` updates `enabledOAuthProviders`, so the provider buttons appear without editing the form component.
