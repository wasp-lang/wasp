import { ForgotPasswordForm } from "@wasp.sh/auth/client";
import { FeatureContainer } from "../../../components/FeatureContainer";
import { customisationProps } from "./auth-ui";

export function RequestPasswordReset() {
  return (
    <FeatureContainer>
      <div className="mx-auto max-w-sm">
        <ForgotPasswordForm {...customisationProps} />
      </div>
    </FeatureContainer>
  );
}
