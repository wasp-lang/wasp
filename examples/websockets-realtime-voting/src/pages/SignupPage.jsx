import { useNavigate } from "react-router";
import { login, signup, useSignupForm } from "wasp/client/auth";

export function SignupPage() {
  const navigate = useNavigate();
  const form = useSignupForm({
    identityField: "username",
    async submit(fields) {
      await signup(fields);
      await login(fields);
    },
    onSuccess() {
      navigate("/");
    },
  });

  return (
    <form className="mx-auto max-w-sm" onSubmit={(event) => void form.submit(event)}>
      {form.errorMessage && <p>{formatError(form.errorMessage)}</p>}
      <h1 className="mb-4 text-2xl font-bold">Sign up</h1>
      <input
        {...form.getFieldProps("username")}
        className="mb-2 w-full rounded border px-3 py-2"
        placeholder="Username"
        type="text"
      />
      <input
        {...form.getFieldProps("password")}
        className="mb-4 w-full rounded border px-3 py-2"
        placeholder="Password"
        type="password"
      />
      <button
        className="w-full rounded bg-blue-600 px-4 py-2 text-white disabled:opacity-50"
        type="submit"
        disabled={form.isSubmitting}
      >
        Sign up
      </button>
    </form>
  );
}

function formatError(errorMessage) {
  return [errorMessage.title, errorMessage.description].filter(Boolean).join(": ");
}
