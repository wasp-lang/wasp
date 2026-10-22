import { Link } from "react-router";
import { SignupForm } from "wasp/client/auth";
import { AuthLayout } from "../AuthLayout";

export function SignupPage() {
  return (
    <AuthLayout>
      <SignupForm
        additionalFields={[
          {
            name: "username",
            type: "input",
            label: "Username",
            validations: {
              required: "Username is required",
              minLength: {
                value: 6,
                message: "Username must be at least 6 characters long",
              },
            },
          },
        ]}
      />
      <br />
      <span className="text-sm">
        Already have an account?{" "}
        <Link to="/login" className="font-bold underline">
          Go to login
        </Link>
        .
      </span>
    </AuthLayout>
  );
}
