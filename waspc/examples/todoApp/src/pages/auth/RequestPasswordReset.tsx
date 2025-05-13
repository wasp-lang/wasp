import { ForgotPasswordForm } from 'wasp/client/auth'
import todoLogo from '../../todoLogo.png'
import appearance from './appearance'

export function RequestPasswordReset() {
  return (
    <div className='h-full w-full bg-white'>
      <div className='flex min-h-[75vh] min-w-full items-center justify-center'>
        <div className='h-full w-full max-w-sm bg-white p-5'>
          <div>
            <ForgotPasswordForm
              appearance={appearance}
              logo={todoLogo}
              socialLayout='horizontal'
            />
          </div>
        </div>
      </div>
    </div>
  )
}
