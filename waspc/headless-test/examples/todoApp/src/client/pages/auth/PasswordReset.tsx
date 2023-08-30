import { Link } from '@wasp/router'

import { ResetPasswordForm } from '@wasp/auth/forms/ResetPassword'
import appearance from './appearance'
import todoLogo from '../../todoLogo.png'

export function PasswordReset() {
  return (
    <div className="w-full h-full bg-white">
      <div className="min-w-full min-h-[75vh] flex items-center justify-center">
        <div className="w-full h-full max-w-sm p-5 bg-white">
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
    </div>
  )
}
