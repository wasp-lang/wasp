import type { ComponentPropsWithoutRef, ReactNode } from "react";
import { useLocation, useNavigate } from "react-router";
import {
  login,
  requestPasswordReset,
  resetPassword,
  signup,
  useForgotPasswordForm,
  useLoginForm,
  useResetPasswordForm,
  useSignupForm,
  useVerifyEmail,
  verifyEmail,
  type ErrorMessage,
} from "wasp/client/auth";

const onAuthSucceededRedirectTo = "/";

type SignupFields = {
  email: string;
  password: string;
  username: string;
};

export function LoginForm() {
  const navigate = useNavigate();
  const form = useLoginForm({
    identityField: "email",
    submit: login,
    onSuccess() {
      navigate(onAuthSucceededRedirectTo);
    },
  });

  return (
    <AuthSection title="Log in to your account">
      <AuthNotice
        errorMessage={form.errorMessage}
        successMessage={form.successMessage}
      />
      <form className="mt-6 space-y-5" onSubmit={(event) => void form.submit(event)}>
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
    </AuthSection>
  );
}

export function SignupForm() {
  const form = useSignupForm<SignupFields>({
    identityField: "email",
    initialFields: { username: "" },
    async submit(fields) {
      await signup(fields);
    },
    resetOnSuccess: true,
    successMessage:
      "You've signed up successfully! Check your email for the confirmation link.",
    validate(fields) {
      if (!fields.username.trim()) {
        return { fieldErrors: { username: "Username is required" } };
      }
      if (fields.username.length < 6) {
        return {
          fieldErrors: {
            username: "Username must be at least 6 characters long",
          },
        };
      }
      return null;
    },
  });

  return (
    <AuthSection title="Create a new account">
      <AuthNotice
        errorMessage={form.errorMessage}
        successMessage={form.successMessage}
      />
      <form className="mt-6 space-y-5" onSubmit={(event) => void form.submit(event)}>
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
        <Field
          {...form.getFieldProps("username")}
          label="Username"
          autoComplete="username"
          error={form.fieldErrors.username}
        />
        <SubmitButton isSubmitting={form.isSubmitting}>Sign up</SubmitButton>
      </form>
    </AuthSection>
  );
}

export function ForgotPasswordForm() {
  const form = useForgotPasswordForm({ submit: requestPasswordReset });

  return (
    <AuthSection title="Forgot your password?">
      <AuthNotice
        errorMessage={form.errorMessage}
        successMessage={form.successMessage}
      />
      <form className="mt-6 space-y-5" onSubmit={(event) => void form.submit(event)}>
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
    </AuthSection>
  );
}

export function ResetPasswordForm() {
  const location = useLocation();
  const token = new URLSearchParams(location.search).get("token");
  const form = useResetPasswordForm({
    token,
    submit: ({ password, token: resetToken }) =>
      resetPassword({ password, token: resetToken }),
  });

  return (
    <AuthSection title="Reset your password">
      <AuthNotice
        errorMessage={form.errorMessage}
        successMessage={form.successMessage}
      />
      <form className="mt-6 space-y-5" onSubmit={(event) => void form.submit(event)}>
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
    </AuthSection>
  );
}

export function VerifyEmailForm() {
  const location = useLocation();
  const token = new URLSearchParams(location.search).get("token");
  const form = useVerifyEmail({ token, verify: verifyEmail });

  return (
    <AuthSection title="Email verification">
      <AuthNotice
        loadingMessage={form.isSubmitting ? "Verifying email..." : null}
        errorMessage={form.errorMessage}
        successMessage={form.successMessage}
      />
    </AuthSection>
  );
}

function AuthSection({ children, title }: { children: ReactNode; title: string }) {
  return (
    <section>
      <h1 className="text-2xl font-semibold tracking-tight text-neutral-900">
        {title}
      </h1>
      {children}
    </section>
  );
}

function AuthNotice({
  errorMessage,
  loadingMessage,
  successMessage,
}: {
  errorMessage?: ErrorMessage | null;
  loadingMessage?: string | null;
  successMessage?: string | null;
}) {
  if (errorMessage) {
    return (
      <div
        className="mt-4 rounded-md border border-red-200 bg-red-50 px-3 py-2 text-sm font-medium text-red-700"
        role="alert"
      >
        {formatError(errorMessage)}
      </div>
    );
  }

  if (successMessage) {
    return (
      <div
        className="mt-4 rounded-md border border-green-200 bg-green-50 px-3 py-2 text-sm font-medium text-green-700"
        role="status"
      >
        {successMessage}
      </div>
    );
  }

  if (loadingMessage) {
    return (
      <div className="mt-4 rounded-md border border-neutral-200 bg-neutral-50 px-3 py-2 text-sm font-medium text-neutral-700">
        {loadingMessage}
      </div>
    );
  }

  return null;
}

type FieldProps = ComponentPropsWithoutRef<"input"> & {
  error?: string;
  label: string;
};

function Field({ error, label, ...inputProps }: FieldProps) {
  return (
    <label className="block">
      <span className="mb-1.5 block text-sm font-medium text-neutral-700">
        {label}
      </span>
      <input
        {...inputProps}
        className="block w-full rounded-md border border-neutral-300 bg-white px-3 py-2 text-sm text-neutral-900 shadow-xs outline-hidden transition focus:border-yellow-500 focus:ring-2 focus:ring-yellow-500 disabled:cursor-not-allowed disabled:bg-neutral-100 disabled:text-neutral-500 disabled:opacity-60"
      />
      {error && (
        <span className="mt-1.5 block text-sm font-medium text-red-600">
          {error}
        </span>
      )}
    </label>
  );
}

function SubmitButton({
  children,
  isSubmitting,
}: {
  children: ReactNode;
  isSubmitting: boolean;
}) {
  return (
    <button
      className="flex w-full justify-center rounded-md border border-yellow-400 bg-yellow-400 px-4 py-2 text-sm font-semibold text-neutral-950 shadow-xs transition hover:bg-yellow-300 disabled:cursor-not-allowed disabled:border-neutral-200 disabled:bg-neutral-100 disabled:text-neutral-500 disabled:opacity-60"
      type="submit"
      disabled={isSubmitting}
    >
      {isSubmitting ? "Loading..." : children}
    </button>
  );
}

function formatError(errorMessage: ErrorMessage) {
  return [errorMessage.title, errorMessage.description].filter(Boolean).join(": ");
}
