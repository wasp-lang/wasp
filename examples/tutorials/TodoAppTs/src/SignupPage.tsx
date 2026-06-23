import { Link, useNavigate } from "react-router";
import {
  login,
  signup,
  useSignupForm,
  type ErrorMessage,
} from "wasp/client/auth";

type AuthFields = {
  username: string;
  password: string;
};

export const SignupPage = () => {
  const navigate = useNavigate();
  const form = useSignupForm<AuthFields>({
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
    <div style={{ maxWidth: "400px", margin: "0 auto" }}>
      <form onSubmit={(event) => void form.submit(event)}>
        {form.errorMessage && <p>{formatError(form.errorMessage)}</p>}
        <h1>Sign up</h1>
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
          Sign up
        </button>
      </form>
      <br />
      <span>
        I already have an account (<Link to="/login">go to login</Link>).
      </span>
    </div>
  );
};

function formatError(errorMessage: ErrorMessage) {
  return [errorMessage.title, errorMessage.description].filter(Boolean).join(": ");
}
