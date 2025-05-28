import { FormItemGroup, SignupForm } from "wasp/client/auth";

import { Link } from "wasp/client/router";

import { customisationProps } from "./auth-ui";

const Signup = () => {
  return (
    <div className="min-w-full flex items-center justify-center">
      <div className="w-full h-full max-w-sm p-5">
        <div>
          <SignupForm
            {...customisationProps}
            additionalFields={[
              {
                name: "address",
                type: "input",
                label: "Address",
                validations: {
                  required: "Address is required",
                },
              },
              () => (
                <FormItemGroup className="text-sm text-gray-500">
                  👉 Don't forget to press the button below to submit the form.
                </FormItemGroup>
              ),
            ]}
          />
          <br />
          <span className="text-sm font-medium text-gray-900">
            You already have an account?{" "}
            <Link to="/login" className="link">
              go to login
            </Link>
            .
          </span>
          <br />
        </div>
      </div>
    </div>
  );
};

export default Signup;
