import { FormItemGroup, SignupForm } from "wasp/client/auth";

import { Link } from "wasp/client/router";

import todoLogo from "../../todoLogo.png";
import appearance from "./appearance";

const Signup = () => {
  return (
    <div className="min-w-full min-h-[75vh] flex items-center justify-center">
      <div className="w-full h-full max-w-sm p-5">
        <div>
          <SignupForm
            appearance={appearance}
            logo={todoLogo}
            socialLayout="horizontal"
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
            I already have an account (<Link to="/login">go to login</Link>).
          </span>
          <br />
        </div>
      </div>
    </div>
  );
};

export default Signup;
