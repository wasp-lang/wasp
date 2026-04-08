import { FormItemGroup, SignupForm } from "wasp/client/auth";

import { Link } from "wasp/client/router";

import { FeatureContainer } from "../../../components/FeatureContainer";
import { customisationProps } from "./auth-ui";

const Signup = () => {
  return (
    <FeatureContainer>
      <div className="mx-auto max-w-sm">
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
            Go to login
          </Link>
          .
        </span>
        <br />
      </div>
    </FeatureContainer>
  );
};

export default Signup;
