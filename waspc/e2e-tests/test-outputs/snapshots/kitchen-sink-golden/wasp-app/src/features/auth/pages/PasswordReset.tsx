import { Link } from "wasp/client/router";

import { ResetPasswordForm } from "wasp/client/auth";
import { FeatureContainer } from "../../../components/FeatureContainer";
import { customisationProps } from "./auth-ui";

export function PasswordReset() {
  return (
    <FeatureContainer>
      <div className="mx-auto max-w-sm">
        <ResetPasswordForm {...customisationProps} />
        <br />
        <span className="text-sm font-medium text-gray-900">
          If everything is okay,{" "}
          <Link to="/login" className="link">
            Go to login
          </Link>
        </span>
      </div>
    </FeatureContainer>
  );
}
