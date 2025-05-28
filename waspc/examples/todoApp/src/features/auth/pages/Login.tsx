import { Link } from "wasp/client/router";

import { LoginForm } from "wasp/client/auth";

import { customisationProps } from "./auth-ui";

const Login = () => {
  return (
    <div className="min-w-full flex items-center justify-center">
      <div className="w-full h-full max-w-sm p-5">
        <div>
          <LoginForm {...customisationProps} />
          <br />
          <span className="text-sm font-medium text-gray-900">
            Don't have an account yet?{" "}
            <Link to="/signup" className="link">
              go to signup
            </Link>
            .
          </span>
          <br />
          <span className="text-sm font-medium text-gray-900">
            Forgot your password?{" "}
            <Link to="/request-password-reset" className="link">
              reset it
            </Link>
            .
          </span>
        </div>
      </div>
    </div>
  );
};

export default Login;
