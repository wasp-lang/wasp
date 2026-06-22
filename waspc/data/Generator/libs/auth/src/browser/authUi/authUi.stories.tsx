import type { Meta, StoryObj } from "@storybook/react-vite";
import { expect, fn, waitFor } from "storybook/test";
import {
  providerIconById,
  useForgotPasswordForm,
  useLoginForm,
  useOAuthProviderActions,
  useResetPasswordForm,
  useSignupForm,
  useVerifyEmail,
  type OAuthProvider,
} from "../index";

type LoginFields = {
  email: string;
  password: string;
};

type SignupFields = LoginFields & {
  displayName: string;
};

type StoryArgs = {
  shouldFail: boolean;
  onLoginSubmit: (fields: LoginFields) => Promise<void> | void;
  onProviderStart: (provider: OAuthProvider) => Promise<void> | void;
};

const meta = {
  title: "Auth/Examples",
  component: RawHeadlessExample,
  args: {
    shouldFail: false,
    onLoginSubmit: fn(),
    onProviderStart: fn(),
  },
} satisfies Meta<typeof RawHeadlessExample>;

export default meta;

type Story = StoryObj<typeof meta>;

export const RawHeadless: Story = {
  play: async ({ args, canvas, step, userEvent }) => {
    await step("Submit password login", async () => {
      await userEvent.type(canvas.getByLabelText("Email"), "miho@example.com");
      await userEvent.type(canvas.getByLabelText("Password"), "super-secret");
      await userEvent.click(canvas.getByRole("button", { name: "Log in" }));
      await waitFor(() => expect(args.onLoginSubmit).toHaveBeenCalled());
      await canvas.findByRole("status");
    });

    await step("Start OAuth provider", async () => {
      await userEvent.click(
        canvas.getByRole("button", { name: "Continue with Google" }),
      );
      await waitFor(() => expect(args.onProviderStart).toHaveBeenCalled());
    });
  },
};

export const ErrorState: Story = {
  args: {
    shouldFail: true,
  },
  play: async ({ canvas, userEvent }) => {
    await userEvent.type(canvas.getByLabelText("Email"), "miho@example.com");
    await userEvent.type(canvas.getByLabelText("Password"), "wrong-password");
    await userEvent.click(canvas.getByRole("button", { name: "Log in" }));
    await canvas.findByRole("alert");
  },
};

export const TailwindExample: Story = {
  render: (args) => <TailwindLoginExample {...args} />,
};

export const PlainCssExample: Story = {
  render: (args) => <PlainCssCurrentFormsExample {...args} />,
};

function RawHeadlessExample({
  shouldFail,
  onLoginSubmit,
  onProviderStart,
}: StoryArgs) {
  const login = useLoginForm<LoginFields>({
    identityField: "email",
    submit: async (fields) => {
      await onLoginSubmit(fields);
      if (shouldFail) {
        throw new Error("Invalid credentials");
      }
      return { successMessage: "Logged in successfully." };
    },
  });
  const oauth = useOAuthProviderActions({
    providers: createOAuthProviders(onProviderStart),
  });

  return (
    <main style={{ display: "grid", gap: "1rem", maxWidth: "24rem" }}>
      <h1>Raw headless auth</h1>
      <form onSubmit={(event) => void login.submit(event)}>
        <label>
          Email
          <input type="email" {...login.getFieldProps("email")} />
        </label>
        {login.fieldErrors.email && <p>{login.fieldErrors.email}</p>}
        <label>
          Password
          <input type="password" {...login.getFieldProps("password")} />
        </label>
        {login.fieldErrors.password && <p>{login.fieldErrors.password}</p>}
        {login.errorMessage && <p role="alert">{login.errorMessage.title}</p>}
        {login.successMessage && <p role="status">{login.successMessage}</p>}
        <button type="submit" disabled={login.isSubmitting}>
          Log in
        </button>
      </form>
      <ProviderButtons providers={oauth.providers} />
    </main>
  );
}

function TailwindLoginExample({ onLoginSubmit, onProviderStart }: StoryArgs) {
  const login = useLoginForm<LoginFields>({
    identityField: "email",
    submit: async (fields) => {
      await onLoginSubmit(fields);
      return { successMessage: "Logged in successfully." };
    },
  });
  const oauth = useOAuthProviderActions({
    providers: createOAuthProviders(onProviderStart),
  });

  return (
    <main className="mx-auto grid max-w-md gap-6 rounded-2xl border border-slate-200 bg-white p-8 shadow-sm">
      <header className="space-y-2 text-center">
        <h1 className="text-2xl font-semibold tracking-tight text-slate-950">
          Log in to your account
        </h1>
        <p className="text-sm text-slate-500">Email and OAuth login</p>
      </header>
      <ProviderButtons
        providers={oauth.providers}
        className="grid gap-2"
        buttonClassName="inline-flex h-11 w-full items-center justify-center gap-2 rounded-lg border border-slate-200 px-3 text-sm font-medium text-slate-700 transition hover:bg-slate-50 disabled:opacity-60"
      />
      <form
        className="grid gap-4"
        onSubmit={(event) => void login.submit(event)}
      >
        <label className="grid gap-1 text-sm font-medium text-slate-700">
          Email
          <input
            className="h-11 rounded-lg border border-slate-300 px-3 text-slate-950"
            type="email"
            {...login.getFieldProps("email")}
          />
        </label>
        <label className="grid gap-1 text-sm font-medium text-slate-700">
          Password
          <input
            className="h-11 rounded-lg border border-slate-300 px-3 text-slate-950"
            type="password"
            {...login.getFieldProps("password")}
          />
        </label>
        {login.errorMessage && (
          <p
            className="rounded-lg bg-red-50 p-3 text-sm text-red-700"
            role="alert"
          >
            {login.errorMessage.title}
          </p>
        )}
        <button
          className="h-11 rounded-lg bg-slate-950 px-4 font-medium text-white disabled:opacity-60"
          type="submit"
          disabled={login.isSubmitting}
        >
          Log in
        </button>
      </form>
    </main>
  );
}

function PlainCssCurrentFormsExample({ onProviderStart }: StoryArgs) {
  const signup = useSignupForm<SignupFields>({
    identityField: "email",
    initialFields: { displayName: "" },
    submit: async () => ({
      successMessage: "Check your email to confirm signup.",
    }),
  });
  const forgotPassword = useForgotPasswordForm({
    submit: async () => undefined,
  });
  const resetPassword = useResetPasswordForm({
    token: "reset-token",
    submit: async () => ({ successMessage: "Password reset." }),
  });
  const verifyEmail = useVerifyEmail({
    token: "verify-token",
    verify: async () => ({ successMessage: "Email verified." }),
  });
  const oauth = useOAuthProviderActions({
    providers: createOAuthProviders(onProviderStart),
  });

  return (
    <main className="auth-demo">
      <style>{plainCss}</style>
      <section className="auth-card">
        <h1>Create a new account</h1>
        <ProviderButtons
          providers={oauth.providers}
          className="provider-grid"
          buttonClassName="provider-button"
        />
        <form onSubmit={(event) => void signup.submit(event)}>
          <label>
            Display name
            <input {...signup.getFieldProps("displayName")} />
          </label>
          <label>
            Email
            <input type="email" {...signup.getFieldProps("email")} />
          </label>
          <label>
            Password
            <input type="password" {...signup.getFieldProps("password")} />
          </label>
          {signup.successMessage && (
            <p role="status">{signup.successMessage}</p>
          )}
          <button type="submit" disabled={signup.isSubmitting}>
            Sign up
          </button>
        </form>
      </section>
      <section className="auth-card compact">
        <h2>Forgot password</h2>
        <form onSubmit={(event) => void forgotPassword.submit(event)}>
          <label>
            Email
            <input type="email" {...forgotPassword.getFieldProps("email")} />
          </label>
          {forgotPassword.successMessage && (
            <p role="status">{forgotPassword.successMessage}</p>
          )}
          <button type="submit" disabled={forgotPassword.isSubmitting}>
            Send reset email
          </button>
        </form>
      </section>
      <section className="auth-card compact">
        <h2>Reset password</h2>
        <form onSubmit={(event) => void resetPassword.submit(event)}>
          <label>
            New password
            <input
              type="password"
              {...resetPassword.getFieldProps("password")}
            />
          </label>
          <label>
            Confirm password
            <input
              type="password"
              {...resetPassword.getFieldProps("passwordConfirmation")}
            />
          </label>
          <button type="submit" disabled={resetPassword.isSubmitting}>
            Reset password
          </button>
        </form>
      </section>
      <section className="auth-card compact">
        <h2>Email verification</h2>
        {verifyEmail.isSubmitting && (
          <p className="message muted">Verifying email...</p>
        )}
        {verifyEmail.successMessage && (
          <p className="message success" role="status">
            {verifyEmail.successMessage}
          </p>
        )}
      </section>
    </main>
  );
}

function ProviderButtons({
  providers,
  className,
  buttonClassName,
}: {
  providers: ReturnType<typeof useOAuthProviderActions>["providers"];
  className?: string;
  buttonClassName?: string;
}) {
  return (
    <div className={className}>
      {providers.map((provider) => {
        const Icon = providerIconById[provider.id];
        return (
          <button
            key={provider.id}
            className={buttonClassName}
            {...provider.getButtonProps()}
          >
            {Icon && <Icon width="1.25em" height="1.25em" />}
            <span>Continue with {provider.label}</span>
          </button>
        );
      })}
    </div>
  );
}

function createOAuthProviders(
  onProviderStart: StoryArgs["onProviderStart"],
): OAuthProvider[] {
  return [
    { id: "google", label: "Google", start: onProviderStart },
    { id: "github", label: "GitHub", start: onProviderStart },
    { id: "discord", label: "Discord", start: onProviderStart },
  ];
}

const plainCss = `
.auth-demo {
  display: grid;
  gap: 1rem;
  max-width: 28rem;
  color: #0f172a;
  font-family: Inter, ui-sans-serif, system-ui, sans-serif;
}

.auth-card {
  display: grid;
  gap: 1rem;
  padding: 1.5rem;
  border: 1px solid #e2e8f0;
  border-radius: 1rem;
  background: white;
  box-shadow: 0 16px 40px rgb(15 23 42 / 8%);
}

.auth-card.compact {
  padding: 1rem;
}

.auth-card h1,
.auth-card h2 {
  margin: 0;
}

.auth-card form,
.provider-grid {
  display: grid;
  gap: .75rem;
}

.message {
  margin: 0;
  padding: .75rem .9rem;
  border-radius: .7rem;
  font-size: .95rem;
}

.message.success {
  border: 1px solid #bbf7d0;
  background: #f0fdf4;
  color: #166534;
}

.message.muted {
  border: 1px solid #cbd5e1;
  background: #f8fafc;
  color: #475569;
}

.provider-button {
  display: inline-flex;
  align-items: center;
  justify-content: center;
  gap: .5rem;
  width: 100%;
  min-height: 2.75rem;
  border: 1px solid #cbd5e1;
  border-radius: .7rem;
  background: white;
  color: #0f172a;
  font-weight: 700;
}

.provider-button svg {
  flex: 0 0 auto;
}

.auth-card label {
  display: grid;
  gap: .35rem;
  font-size: .875rem;
  font-weight: 600;
}

.auth-card input,
.auth-card form > button {
  min-height: 2.75rem;
  border-radius: .7rem;
}

.auth-card input {
  border: 1px solid #cbd5e1;
  padding: 0 .8rem;
}

.auth-card form > button {
  border: 1px solid #0f172a;
  background: #0f172a;
  color: white;
  font-weight: 700;
}
`;
