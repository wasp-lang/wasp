import { Link } from "wasp/client/router";

import { FeatureContainer } from "../../../components/FeatureContainer";
import { VerifyEmailForm } from "./AuthForms";

export function EmailVerification() {
  return (
      <FeatureContainer>
        <div className="mx-auto max-w-sm">
        <VerifyEmailForm />
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
