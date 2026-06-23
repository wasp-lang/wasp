import { Link } from "wasp/client/router";

import { FeatureContainer } from "../../../components/FeatureContainer";
import { ResetPasswordForm } from "./AuthForms";

export function PasswordReset() {
  return (
      <FeatureContainer>
        <div className="mx-auto max-w-sm">
        <ResetPasswordForm />
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
