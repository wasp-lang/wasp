import { Link } from "wasp/client/router";

import { FeatureContainer } from "../../../components/FeatureContainer";
import { SignupForm } from "./AuthForms";

const Signup = () => {
  return (
      <FeatureContainer>
        <div className="mx-auto max-w-sm">
        <SignupForm />
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
