import { Link } from 'react-router-dom'

import { SignupForm } from '@wasp/auth/forms/Signup'

const SignupPage = () => {
  return (
    <>
      <div style={{maxWidth: "400px", margin: "0 auto"}}>
        <SignupForm/>
        <br/>
        <span>
          I already have an account (<Link to="/login">go to login</Link>).
        </span>
      </div>
    </>
  )
}

export default SignupPage