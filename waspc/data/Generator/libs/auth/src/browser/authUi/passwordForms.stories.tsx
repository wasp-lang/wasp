import type { Meta, StoryObj } from "@storybook/react-vite";
import { useState } from "react";
import {
  useForgotPasswordForm,
  useLoginForm,
  useResetPasswordForm,
  useSignupForm,
  useVerifyEmail,
  type AuthFormFieldProps,
  type AuthFormSubmitResult,
} from "../index";

type PasswordFormController<Fields extends Record<string, string>> = {
  fieldErrors: Partial<Record<keyof Fields & string, string>>;
  successMessage: string | null;
  isSubmitting: boolean;
  getFieldProps: (name: keyof Fields & string) => AuthFormFieldProps;
  submit: (event?: React.FormEvent<HTMLFormElement>) => Promise<unknown>;
};

const meta = {
  title: "Auth/Hooks/Password Forms",
} satisfies Meta;

export default meta;

type Story = StoryObj<typeof meta>;

type LoginFields = {
  email: string;
  password: string;
};

type SignupFields = LoginFields & {
  displayName: string;
};

export const Login: Story = {
  render: () => <LoginHarness />,
};

export const ExternalFormController: Story = {
  render: () => <ExternalFormControllerHarness />,
};

export const Signup: Story = {
  render: () => <SignupHarness />,
};

export const ForgotPassword: Story = {
  render: () => <ForgotPasswordHarness />,
};

export const ResetPassword: Story = {
  render: () => <ResetPasswordHarness />,
};

export const VerifyEmail: Story = {
  render: () => <VerifyEmailHarness />,
};

function LoginHarness() {
  const form = useLoginForm<LoginFields>({
    identityField: "email",
    submit: async () => ({ successMessage: "Logged in." }),
  });

  return (
    <PasswordForm<LoginFields>
      title="Login"
      buttonLabel="Log in"
      form={form}
      identityField="email"
    />
  );
}

function ExternalFormControllerHarness() {
  const [fields, setFields] = useState<LoginFields>({
    email: "miho@example.com",
    password: "super-secret",
  });
  const form = useLoginForm<LoginFields>({
    identityField: "email",
    submit: async () => ({ successMessage: "Submitted external fields." }),
  });

  return (
    <form
      className="grid max-w-sm gap-3"
      onSubmit={(event) => {
        event.preventDefault();
        void form.submitFields(fields);
      }}
    >
      <h2 className="text-xl font-semibold">External form controller</h2>
      <label className="grid gap-1">
        Email
        <input
          className="rounded border px-3 py-2"
          value={fields.email}
          onChange={(event) =>
            setFields((currentFields) => ({
              ...currentFields,
              email: event.target.value,
            }))
          }
        />
      </label>
      <label className="grid gap-1">
        Password
        <input
          className="rounded border px-3 py-2"
          type="password"
          value={fields.password}
          onChange={(event) =>
            setFields((currentFields) => ({
              ...currentFields,
              password: event.target.value,
            }))
          }
        />
      </label>
      {form.errorMessage && (
        <p className="text-red-700" role="alert">
          {form.errorMessage.title}
        </p>
      )}
      {form.successMessage && (
        <p className="text-green-700" role="status">
          {form.successMessage}
        </p>
      )}
      <button
        className="rounded bg-slate-950 px-4 py-2 text-white"
        type="submit"
        disabled={form.isSubmitting}
      >
        Submit external fields
      </button>
    </form>
  );
}

function SignupHarness() {
  const form = useSignupForm<SignupFields>({
    identityField: "email",
    initialFields: { displayName: "" },
    submit: async () => ({ successMessage: "Signup submitted." }),
  });

  return (
    <PasswordForm<SignupFields>
      title="Signup"
      buttonLabel="Sign up"
      form={form}
      identityField="email"
      extraField={{ name: "displayName", label: "Display name" }}
    />
  );
}

function ForgotPasswordHarness() {
  const form = useForgotPasswordForm({
    submit: async () => undefined,
  });

  return (
    <form
      className="grid max-w-sm gap-3"
      onSubmit={(event) => void form.submit(event)}
    >
      <h2 className="text-xl font-semibold">Forgot password</h2>
      <label className="grid gap-1">
        Email
        <input
          className="rounded border px-3 py-2"
          type="email"
          {...form.getFieldProps("email")}
        />
      </label>
      {form.successMessage && (
        <p className="text-green-700" role="status">
          {form.successMessage}
        </p>
      )}
      <button
        className="rounded bg-slate-950 px-4 py-2 text-white"
        type="submit"
        disabled={form.isSubmitting}
      >
        Send reset email
      </button>
    </form>
  );
}

function ResetPasswordHarness() {
  const form = useResetPasswordForm({
    token: "reset-token",
    submit: async (): Promise<AuthFormSubmitResult> => ({
      successMessage: "Password reset.",
    }),
  });

  return (
    <form
      className="grid max-w-sm gap-3"
      onSubmit={(event) => void form.submit(event)}
    >
      <h2 className="text-xl font-semibold">Reset password</h2>
      <label className="grid gap-1">
        New password
        <input
          className="rounded border px-3 py-2"
          type="password"
          {...form.getFieldProps("password")}
        />
      </label>
      <label className="grid gap-1">
        Confirm password
        <input
          className="rounded border px-3 py-2"
          type="password"
          {...form.getFieldProps("passwordConfirmation")}
        />
      </label>
      {form.fieldErrors.passwordConfirmation && (
        <p className="text-red-700">{form.fieldErrors.passwordConfirmation}</p>
      )}
      {form.errorMessage && (
        <p className="text-red-700" role="alert">
          {form.errorMessage.title}
        </p>
      )}
      {form.successMessage && (
        <p className="text-green-700" role="status">
          {form.successMessage}
        </p>
      )}
      <button
        className="rounded bg-slate-950 px-4 py-2 text-white"
        type="submit"
        disabled={form.isSubmitting}
      >
        Reset password
      </button>
    </form>
  );
}

function VerifyEmailHarness() {
  const form = useVerifyEmail({
    token: "verification-token",
    verify: async () => ({ successMessage: "Email verified." }),
  });

  return (
    <section className="grid max-w-sm gap-3">
      <h2 className="text-xl font-semibold">Verify email</h2>
      {form.isSubmitting && <p>Verifying email...</p>}
      {form.successMessage && (
        <p className="text-green-700" role="status">
          {form.successMessage}
        </p>
      )}
      {form.errorMessage && (
        <p className="text-red-700" role="alert">
          {form.errorMessage.title}
        </p>
      )}
    </section>
  );
}

function PasswordForm<Fields extends Record<string, string>>({
  title,
  buttonLabel,
  form,
  identityField,
  extraField,
}: {
  title: string;
  buttonLabel: string;
  form: PasswordFormController<Fields>;
  identityField: "email" | "username";
  extraField?: { name: keyof Fields & string; label: string };
}) {
  return (
    <form
      className="grid max-w-sm gap-3"
      onSubmit={(event) => void form.submit(event)}
    >
      <h2 className="text-xl font-semibold">{title}</h2>
      {extraField && (
        <label className="grid gap-1">
          {extraField.label}
          <input
            className="rounded border px-3 py-2"
            {...form.getFieldProps(extraField.name)}
          />
        </label>
      )}
      <label className="grid gap-1">
        {identityField === "email" ? "Email" : "Username"}
        <input
          className="rounded border px-3 py-2"
          type={identityField === "email" ? "email" : "text"}
          {...form.getFieldProps(identityField)}
        />
      </label>
      <label className="grid gap-1">
        Password
        <input
          className="rounded border px-3 py-2"
          type="password"
          {...form.getFieldProps("password")}
        />
      </label>
      {form.successMessage && (
        <p className="text-green-700" role="status">
          {form.successMessage}
        </p>
      )}
      <button
        className="rounded bg-slate-950 px-4 py-2 text-white"
        type="submit"
        disabled={form.isSubmitting}
      >
        {buttonLabel}
      </button>
    </form>
  );
}
