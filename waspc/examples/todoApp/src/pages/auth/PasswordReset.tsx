import { Link } from 'wasp/client/router'

import { ResetPasswordForm } from 'wasp/client/auth'
import todoLogo from '../../todoLogo.png'
import appearance from './appearance'

export function PasswordReset() {
  return (
    <div className='h-full w-full bg-white'>
      <div className='flex min-h-[75vh] min-w-full items-center justify-center'>
        <div className='h-full w-full max-w-sm bg-white p-5'>
          <div>
            <ResetPasswordForm
              appearance={appearance}
              logo={todoLogo}
              socialLayout='horizontal'
            />
            <br />
            <span className='text-sm font-medium text-gray-900'>
              If everything is okay, <Link to='/login'>go to login</Link>
            </span>
          </div>
        </div>
      </div>
    </div>
  )
}
