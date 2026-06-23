import type { ComponentPropsWithoutRef, ReactNode } from "react";
import { useLocation, useNavigate } from "react-router";
import {
  enabledOAuthProviders,
  login,
  providerIconById,
  requestPasswordReset,
  resetPassword,
  signup,
  useForgotPasswordForm,
  useLoginForm,
  useOAuthProviderActions,
  useResetPasswordForm,
  useSignupForm,
  useVerifyEmail,
  verifyEmail,
  type ErrorMessage,
} from "wasp/client/auth";
import { cn } from "../../../cn";

const onAuthSucceededRedirectTo = "/";
const hasOAuthProviders = enabledOAuthProviders.length > 0;

type SignupFields = {
  email: string;
  password: string;
  address: string;
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
    <AuthCard title="Log in to your account">
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
    </AuthCard>
  );
}

export function SignupForm() {
  const form = useSignupForm<SignupFields>({
    identityField: "email",
    initialFields: { address: "" },
    async submit(fields) {
      await signup(fields);
    },
    resetOnSuccess: true,
    successMessage:
      "You've signed up successfully! Check your email for the confirmation link.",
  });

  return (
    <AuthCard title="Create a new account">
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
          {...form.getFieldProps("address")}
          label="Address"
          autoComplete="street-address"
          error={form.fieldErrors.address}
        />
        <p className="rounded-lg border border-gray-200 bg-gray-50 px-3 py-2 text-sm text-gray-600">
          Do not forget to press the button below to submit the form.
        </p>
        <SubmitButton isSubmitting={form.isSubmitting}>Sign up</SubmitButton>
      </form>
    </AuthCard>
  );
}

export function ForgotPasswordForm() {
  const form = useForgotPasswordForm({
    submit: requestPasswordReset,
  });

  return (
    <AuthCard title="Forgot your password?">
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
    </AuthCard>
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
    <AuthCard title="Reset your password">
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
    </AuthCard>
  );
}

export function VerifyEmailForm() {
  const location = useLocation();
  const token = new URLSearchParams(location.search).get("token");
  const form = useVerifyEmail({
    token,
    verify: verifyEmail,
  });

  return (
    <AuthCard title="Email verification">
      <AuthNotice
        loadingMessage={form.isSubmitting ? "Verifying email..." : null}
        errorMessage={form.errorMessage}
        successMessage={form.successMessage}
      />
    </AuthCard>
  );
}

function AuthCard({ children, title }: { children: ReactNode; title: string }) {
  return (
    <div className="rounded-2xl border border-gray-200 bg-white p-6 shadow-xs">
      <h1 className="text-2xl font-semibold tracking-tight text-gray-900">
        {title}
      </h1>
      {children}
    </div>
  );
}

function OAuthButtons({ action }: { action: "Log in" | "Sign up" }) {
  const socialAuth = useOAuthProviderActions({ providers: enabledOAuthProviders });

  return (
    <div className="mt-6">
      <p className="text-sm font-medium text-gray-700">{action} with</p>
      <AuthNotice errorMessage={socialAuth.errorMessage} />
      <div className="mt-2 grid grid-cols-5 gap-2">
        {socialAuth.providers.map((provider) => {
          const ProviderIcon = providerIconById[provider.id];
          if (!ProviderIcon) {
            return null;
          }
          return (
            <a
              key={provider.id}
              {...provider.getLinkProps()}
              className="flex h-10 items-center justify-center rounded-lg border border-gray-300 bg-gray-50 text-gray-700 shadow-xs transition hover:bg-gray-100 aria-disabled:pointer-events-none aria-disabled:opacity-50"
              aria-label={`${action} with ${provider.label}`}
            >
              <ProviderIcon className="h-5 w-5" />
            </a>
          );
        })}
      </div>
    </div>
  );
}

function AuthDivider() {
  return (
    <div className="relative mt-6">
      <div className="absolute inset-0 flex items-center" aria-hidden="true">
        <div className="w-full border-t border-gray-200" />
      </div>
      <div className="relative flex justify-center text-sm">
        <span className="bg-white px-2 text-gray-500">Or continue with</span>
      </div>
    </div>
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
        className="mt-4 rounded-lg border border-red-200 bg-red-50 px-3 py-2 text-sm font-medium text-red-700"
        role="alert"
      >
        {formatError(errorMessage)}
      </div>
    );
  }

  if (successMessage) {
    return (
      <div
        className="mt-4 rounded-lg border border-green-200 bg-green-50 px-3 py-2 text-sm font-medium text-green-700"
        role="status"
      >
        {successMessage}
      </div>
    );
  }

  if (loadingMessage) {
    return (
      <div className="mt-4 rounded-lg border border-gray-200 bg-gray-50 px-3 py-2 text-sm font-medium text-gray-700">
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

function Field({ className, error, label, ...inputProps }: FieldProps) {
  return (
    <label className="block">
      <span className="mb-1.5 block text-sm font-medium text-gray-700">
        {label}
      </span>
      <input
        {...inputProps}
        className={cn(
          "block w-full rounded-lg border border-gray-300 bg-white px-3 py-2 text-sm text-gray-900 shadow-xs outline-hidden transition",
          "focus:border-primary-500 focus:ring-2 focus:ring-primary-500",
          "disabled:cursor-not-allowed disabled:bg-gray-100 disabled:text-gray-500 disabled:opacity-60",
          className,
        )}
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
      className="flex w-full justify-center rounded-lg border border-primary-500 bg-primary-500 px-4 py-2 text-sm font-semibold text-gray-900 shadow-xs transition hover:bg-primary-400 disabled:cursor-not-allowed disabled:border-gray-200 disabled:bg-gray-100 disabled:text-gray-500 disabled:opacity-60"
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
