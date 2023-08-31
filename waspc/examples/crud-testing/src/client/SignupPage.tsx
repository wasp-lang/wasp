import { SignupForm } from "@wasp/auth/forms/Signup";
import {
  FormError,
  FormInput,
  FormItemGroup,
  FormLabel,
} from "@wasp/auth/forms/internal/Form";

export const SignupPage = () => {
  return (
    <div className="container">
      <main>
        <h1>Signup</h1>
        <SignupForm
          additionalFields={[
            {
              name: "address",
              label: "Address",
              type: "input",
              validations: {
                required: "Address is required",
              },
            },
          ]}
        />
      </main>
    </div>
  );
};
