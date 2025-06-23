import { ForgotPasswordForm } from "wasp/client/auth";
import { FeatureContainer } from "../../../components/FeatureContainer";
import { customisationProps } from "./auth-ui";

export function RequestPasswordReset() {
  return (
    <FeatureContainer>
      <div className="max-w-sm mx-auto">
        <ForgotPasswordForm {...customisationProps} />
      </div>
    </FeatureContainer>
  );
}
