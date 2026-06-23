import { Link, useNavigate } from "react-router";
import { login, useLoginForm, type ErrorMessage } from "wasp/client/auth";

type AuthFields = {
  username: string;
  password: string;
};

export const LoginPage = () => {
  const navigate = useNavigate();
  const form = useLoginForm<AuthFields>({
    identityField: "username",
    submit: login,
    onSuccess() {
      navigate("/");
    },
  });

  return (
    <div style={{ maxWidth: "400px", margin: "0 auto" }}>
      <form onSubmit={(event) => void form.submit(event)}>
        {form.errorMessage && <p>{formatError(form.errorMessage)}</p>}
        <h1>Log in</h1>
        <input
          {...form.getFieldProps("username")}
          placeholder="Username"
          type="text"
        />
        <br />
        <input
          {...form.getFieldProps("password")}
          placeholder="Password"
          type="password"
        />
        <br />
        <button type="submit" disabled={form.isSubmitting}>
          Log in
        </button>
      </form>
      <br />
      <span>
        I don't have an account yet (<Link to="/signup">go to signup</Link>).
      </span>
    </div>
  );
};

function formatError(errorMessage: ErrorMessage) {
  return [errorMessage.title, errorMessage.description].filter(Boolean).join(": ");
}
