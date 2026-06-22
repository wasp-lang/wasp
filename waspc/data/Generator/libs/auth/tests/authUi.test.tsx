// @vitest-environment jsdom
import "@testing-library/jest-dom/vitest";
import { cleanup, render, screen, waitFor } from "@testing-library/react";
import userEvent from "@testing-library/user-event";
import { afterEach, describe, expect, it, vi } from "vitest";
import {
  useForgotPasswordForm,
  useLoginForm,
  useOAuthProviderActions,
  useResetPasswordForm,
  useVerifyEmail,
  type AuthFormSubmitResult,
  type OAuthProvider,
} from "../src/browser";

afterEach(() => {
  cleanup();
});

describe("headless auth UI", () => {
  it("should validate required login fields", async () => {
    const user = userEvent.setup();
    const submit = vi.fn();

    render(<LoginForm submit={submit} />);

    await user.click(screen.getByRole("button", { name: "Log in" }));

    expect(screen.getByText("Email is required")).toBeInTheDocument();
    expect(screen.getByText("Password is required")).toBeInTheDocument();
    expect(submit).not.toHaveBeenCalled();
  });

  it("should expose loading and success state during submit", async () => {
    const user = userEvent.setup();
    const deferred = createDeferred<AuthFormSubmitResult>();
    const submit = vi.fn(() => deferred.promise);

    render(<LoginForm submit={submit} />);

    await user.type(screen.getByLabelText("Email"), "miho@example.com");
    await user.type(screen.getByLabelText("Password"), "super-secret");
    await user.click(screen.getByRole("button", { name: "Log in" }));

    await waitFor(() =>
      expect(screen.getByRole("button", { name: "Log in" })).toBeDisabled(),
    );
    expect(submit).toHaveBeenCalledWith({
      email: "miho@example.com",
      password: "super-secret",
    });

    deferred.resolve({ successMessage: "Welcome back" });

    expect(await screen.findByRole("status")).toHaveTextContent("Welcome back");
  });

  it("should expose submit errors", async () => {
    const user = userEvent.setup();
    const submit = vi.fn(async () => {
      throw new Error("Invalid credentials");
    });

    render(<LoginForm submit={submit} />);

    await user.type(screen.getByLabelText("Email"), "miho@example.com");
    await user.type(screen.getByLabelText("Password"), "wrong-password");
    await user.click(screen.getByRole("button", { name: "Log in" }));

    expect(await screen.findByRole("alert")).toHaveTextContent(
      "Invalid credentials",
    );
  });

  it("should reset forgot password fields after success", async () => {
    const user = userEvent.setup();
    const submit = vi.fn(async () => undefined);

    render(<ForgotPasswordForm submit={submit} />);

    const emailInput = screen.getByLabelText("Email");
    await user.type(emailInput, "miho@example.com");
    await user.click(
      screen.getByRole("button", { name: "Send password reset email" }),
    );

    expect(await screen.findByRole("status")).toHaveTextContent(
      "Check your email for a password reset link.",
    );
    expect(emailInput).toHaveValue("");
  });

  it("should validate reset password token and confirmation", async () => {
    const user = userEvent.setup();
    const submit = vi.fn();

    render(<ResetPasswordForm token={null} submit={submit} />);

    await user.type(screen.getByLabelText("New password"), "new-password");
    await user.type(screen.getByLabelText("Confirm new password"), "different");
    await user.click(screen.getByRole("button", { name: "Reset password" }));

    expect(await screen.findByRole("alert")).toHaveTextContent(
      "The token is missing from the URL.",
    );
    expect(screen.getByText("Passwords don't match")).toBeInTheDocument();
    expect(submit).not.toHaveBeenCalled();
  });

  it("should verify email automatically when token is present", async () => {
    const verify = vi.fn(async () => ({ successMessage: "Verified" }));

    render(<VerifyEmailStatus token="email-token" verify={verify} />);

    await waitFor(() => {
      expect(verify).toHaveBeenCalledWith({ token: "email-token" });
    });
    expect(await screen.findByRole("status")).toHaveTextContent(
      "Your email has been verified. You can now log in.",
    );
  });

  it("should expose a missing token error when verifying email", async () => {
    const verify = vi.fn(async () => ({ successMessage: "Verified" }));

    render(<VerifyEmailStatus verify={verify} />);

    expect(await screen.findByRole("alert")).toHaveTextContent(
      "The token is missing from the URL.",
    );
    expect(verify).not.toHaveBeenCalled();
  });

  it("should verify a new email token when the token changes", async () => {
    const verify = vi.fn(async () => ({ successMessage: "Verified" }));
    const { rerender } = render(
      <VerifyEmailStatus token={null} verify={verify} />,
    );

    await screen.findByRole("alert");
    rerender(<VerifyEmailStatus token="new-token" verify={verify} />);

    await waitFor(() => {
      expect(verify).toHaveBeenCalledWith({ token: "new-token" });
    });
  });

  it("should call OAuth provider actions and expose active loading state", async () => {
    const user = userEvent.setup();
    const deferred = createDeferred<void>();
    const start = vi.fn(() => deferred.promise);

    render(
      <OAuthProviders providers={[{ id: "google", label: "Google", start }]} />,
    );

    await user.click(
      screen.getByRole("button", { name: "Continue with Google" }),
    );

    await waitFor(() =>
      expect(
        screen.getByRole("button", { name: "Continue with Google" }),
      ).toBeDisabled(),
    );
    expect(start).toHaveBeenCalledWith({
      id: "google",
      label: "Google",
      start,
    });

    deferred.resolve();
    await waitFor(() =>
      expect(
        screen.getByRole("button", { name: "Continue with Google" }),
      ).not.toBeDisabled(),
    );
  });
});

function LoginForm({
  submit,
}: {
  submit: (fields: {
    email: string;
    password: string;
  }) => Promise<AuthFormSubmitResult> | AuthFormSubmitResult;
}) {
  const form = useLoginForm<{ email: string; password: string }>({
    identityField: "email",
    submit,
  });

  return (
    <form aria-label="login form" onSubmit={(event) => void form.submit(event)}>
      <label>
        Email
        <input type="email" {...form.getFieldProps("email")} />
      </label>
      {form.fieldErrors.email && <div>{form.fieldErrors.email}</div>}
      <label>
        Password
        <input type="password" {...form.getFieldProps("password")} />
      </label>
      {form.fieldErrors.password && <div>{form.fieldErrors.password}</div>}
      {form.errorMessage && <div role="alert">{form.errorMessage.title}</div>}
      {form.successMessage && <div role="status">{form.successMessage}</div>}
      <button type="submit" disabled={form.isSubmitting}>
        Log in
      </button>
    </form>
  );
}

function ForgotPasswordForm({
  submit,
}: {
  submit: (fields: {
    email: string;
  }) => Promise<AuthFormSubmitResult> | AuthFormSubmitResult;
}) {
  const form = useForgotPasswordForm({ submit });

  return (
    <form onSubmit={(event) => void form.submit(event)}>
      <label>
        Email
        <input type="email" {...form.getFieldProps("email")} />
      </label>
      {form.successMessage && <div role="status">{form.successMessage}</div>}
      <button type="submit" disabled={form.isSubmitting}>
        Send password reset email
      </button>
    </form>
  );
}

function ResetPasswordForm({
  token,
  submit,
}: {
  token?: string | null;
  submit: (fields: {
    password: string;
    passwordConfirmation: string;
    token: string;
  }) => Promise<AuthFormSubmitResult> | AuthFormSubmitResult;
}) {
  const form = useResetPasswordForm({ token, submit });

  return (
    <form onSubmit={(event) => void form.submit(event)}>
      <label>
        New password
        <input type="password" {...form.getFieldProps("password")} />
      </label>
      <label>
        Confirm new password
        <input
          type="password"
          {...form.getFieldProps("passwordConfirmation")}
        />
      </label>
      {form.fieldErrors.passwordConfirmation && (
        <div>{form.fieldErrors.passwordConfirmation}</div>
      )}
      {form.errorMessage && <div role="alert">{form.errorMessage.title}</div>}
      <button type="submit">Reset password</button>
    </form>
  );
}

function VerifyEmailStatus({
  token,
  verify,
}: {
  token?: string | null;
  verify: (fields: {
    token: string;
  }) => Promise<AuthFormSubmitResult> | AuthFormSubmitResult;
}) {
  const form = useVerifyEmail({ token, verify });

  return (
    <>
      {form.errorMessage && <div role="alert">{form.errorMessage.title}</div>}
      {form.successMessage && <div role="status">{form.successMessage}</div>}
    </>
  );
}

function OAuthProviders({ providers }: { providers: OAuthProvider[] }) {
  const oauth = useOAuthProviderActions({ providers });

  return (
    <div>
      {oauth.providers.map((provider) => (
        <button key={provider.id} {...provider.getButtonProps()}>
          Continue with {provider.label}
        </button>
      ))}
    </div>
  );
}

function createDeferred<T>() {
  let resolve: (value: T | PromiseLike<T>) => void = () => {};
  const promise = new Promise<T>((nextResolve) => {
    resolve = nextResolve;
  });

  return { promise, resolve };
}
