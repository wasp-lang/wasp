import { Link } from 'react-router-dom'

import { LoginForm } from '@wasp/auth/email'
import { SignInButton as GoogleSignInButton } from '@wasp/auth/helpers/Google'
import { SignInButton as GitHubSignInButton } from '@wasp/auth/helpers/GitHub'

const Login = () => {
  return (
    <div className="flex flex-col gap-5">
      <LoginForm />
      <div>
        I don't have an account yet (<Link to="/signup">go to signup</Link>).
      </div>
      <div>
        I forgot my password (
        <Link to="/request-password-reset">reset password</Link>).
      </div>
      <div className="flex flex-col gap-2 max-w-xs">
        <GoogleSignInButton/>
        <GitHubSignInButton/>
      </div>
    </div>
  )
}

export default Login
