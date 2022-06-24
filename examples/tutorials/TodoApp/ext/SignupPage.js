import React from 'react'
import { Link } from 'react-router-dom'

import SignupForm from '@wasp/auth/forms/Signup'
import WaspSourceHeader from './WaspSourceHeader'

const SignupPage = () => {
  return (
    <>
      <WaspSourceHeader name="Todo" />
      <SignupForm/>
      <br/>
      <span>
        I already have an account (<Link to="/login">go to login</Link>).
      </span>
    </>
  )
}

export default SignupPage
