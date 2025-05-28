import { ForgotPasswordForm } from "wasp/client/auth";
import { customisationProps } from "./auth-ui";

export function RequestPasswordReset() {
  return (
    <div className="min-w-full flex items-center justify-center">
      <div className="w-full h-full max-w-sm p-5">
        <div>
          <ForgotPasswordForm {...customisationProps} />
        </div>
      </div>
    </div>
  );
}
