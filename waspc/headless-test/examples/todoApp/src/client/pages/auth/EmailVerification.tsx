import { Link } from 'react-router-dom'

import { VerifyEmailForm } from '@wasp/auth/forms/VerifyEmail'
import appearance from './appearance'
import todoLogo from '../../todoLogo.png'

export function EmailVerification() {
  return (
    <div className="w-full h-full bg-white">
      <div className="min-w-full min-h-[75vh] flex items-center justify-center">
        <div className="w-full h-full max-w-sm p-5 bg-white">
          <div>
            <VerifyEmailForm
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
