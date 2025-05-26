import { ForgotPasswordForm } from "wasp/client/auth";
import todoLogo from "../../todoLogo.png";
import appearance from "./appearance";

export function RequestPasswordReset() {
  return (
    <div className="w-full h-full bg-white">
      <div className="min-w-full min-h-[75vh] flex items-center justify-center">
        <div className="w-full h-full max-w-sm p-5 bg-white">
          <div>
            <ForgotPasswordForm
              appearance={appearance}
              logo={todoLogo}
              socialLayout="horizontal"
            />
          </div>
        </div>
      </div>
    </div>
  );
}
