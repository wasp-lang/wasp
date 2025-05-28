import { Link } from "wasp/client/router";

import { ResetPasswordForm } from "wasp/client/auth";
import todoLogo from "../../todoLogo.png";
import appearance from "./appearance";

export function PasswordReset() {
  return (
    <div className="min-w-full min-h-[75vh] flex items-center justify-center">
      <div className="w-full h-full max-w-sm p-5">
        <div>
          <ResetPasswordForm
            appearance={appearance}
            logo={todoLogo}
            socialLayout="horizontal"
          />
          <br />
          <span className="text-sm font-medium text-gray-900">
            If everything is okay, <Link to="/login">go to login</Link>
          </span>
        </div>
      </div>
    </div>
  );
}
