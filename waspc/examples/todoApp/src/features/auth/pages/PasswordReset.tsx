import { Link } from "wasp/client/router";

import { ResetPasswordForm } from "wasp/client/auth";
import { customisationProps } from "./auth-ui";

export function PasswordReset() {
  return (
    <div className="min-w-full flex items-center justify-center">
      <div className="w-full h-full max-w-sm p-5">
        <div>
          <ResetPasswordForm {...customisationProps} />
          <br />
          <span className="text-sm font-medium text-gray-900">
            If everything is okay,{" "}
            <Link to="/login" className="link">
              go to login
            </Link>
          </span>
        </div>
      </div>
    </div>
  );
}
