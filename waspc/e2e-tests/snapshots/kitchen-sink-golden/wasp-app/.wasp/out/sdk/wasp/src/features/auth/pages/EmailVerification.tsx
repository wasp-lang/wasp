import { Link } from "wasp/client/router";

import { VerifyEmailForm } from "wasp/client/auth";
import { FeatureContainer } from "../../../components/FeatureContainer";
import { customisationProps } from "./auth-ui";

export function EmailVerification() {
  return (
    <FeatureContainer>
      <div className="mx-auto max-w-sm">
        <VerifyEmailForm {...customisationProps} />
        <br />
        <span className="text-sm font-medium text-gray-900">
          If everything is okay,{" "}
          <Link className="link" to="/login">
            Go to login
          </Link>
        </span>
      </div>
    </FeatureContainer>
  );
}
