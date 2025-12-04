import { Link } from "wasp/client/router";

import { LoginForm } from "wasp/client/auth";

import { FeatureContainer } from "../../../components/FeatureContainer";
import { customisationProps } from "./auth-ui";

const Login = () => {
  return (
    <FeatureContainer>
      <div className="mx-auto max-w-sm">
        <LoginForm {...customisationProps} />
        <br />
        <span className="text-sm font-medium text-gray-900">
          Don't have an account yet?{" "}
          <Link to="/signup" className="link">
            Go to signup
          </Link>
          .
        </span>
        <br />
        <span className="text-sm font-medium text-gray-900">
          Forgot your password?{" "}
          <Link to="/request-password-reset" className="link">
            Reset it
          </Link>
          .
        </span>
      </div>
    </FeatureContainer>
  );
};

export default Login;
