import { FeatureContainer } from "../../../components/FeatureContainer";
import { ForgotPasswordForm } from "./AuthForms";

export function RequestPasswordReset() {
  return (
    <FeatureContainer>
      <div className="mx-auto max-w-sm">
        <ForgotPasswordForm />
      </div>
    </FeatureContainer>
  );
}
