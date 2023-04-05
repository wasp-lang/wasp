import { Link } from 'react-router-dom'

import LoginForm from '@wasp/auth/forms/Login'

import appearance from './appearance'
import todoLogo from '../../todoLogo.png'

const Login = () => {
  return (
    <div className="w-full h-full bg-white">
      <div className="min-w-full min-h-[75vh] flex items-center justify-center">
        <div className="w-full h-full max-w-sm p-5 bg-white">
          <div>
            <LoginForm
              appearance={appearance}
              logo={todoLogo}
              socialLayout="horizontal"
            />
            <br />
            <span className="text-sm font-medium text-gray-900">
              Don't have an account yet? (<Link to="/signup">go to signup</Link>
              ).
            </span>
            <br />
          </div>
        </div>
      </div>
    </div>
  )
}

export default Login
