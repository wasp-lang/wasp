import { Link } from 'wasp/client/router'

import { LoginForm } from 'wasp/client/auth'

import todoLogo from '../../todoLogo.png'
import appearance from './appearance'

const Login = () => {
  return (
    <div className='h-full w-full bg-white'>
      <div className='flex min-h-[75vh] min-w-full items-center justify-center'>
        <div className='h-full w-full max-w-sm bg-white p-5'>
          <div>
            <LoginForm
              appearance={appearance}
              logo={todoLogo}
              socialLayout='horizontal'
            />
            <br />
            <span className='text-sm font-medium text-gray-900'>
              Don't have an account yet? <Link to='/signup'>go to signup</Link>.
            </span>
            <br />
            <span className='text-sm font-medium text-gray-900'>
              Forgot your password?{' '}
              <Link to='/request-password-reset'>reset it</Link>.
            </span>
          </div>
        </div>
      </div>
    </div>
  )
}

export default Login
