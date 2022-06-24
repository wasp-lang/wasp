import React from 'react'
import { Link } from 'react-router-dom'

import SignupForm from '@wasp/auth/forms/Signup'
import WaspSourceHeader from './WaspSourceHeader.js'

const SignupPage = (props) => {
  return (
    <>
      <WaspSourceHeader name="Thoughts" />
      <SignupForm/>
      <br/>
      <span>
        I already have an account (<Link to="/login">go to login</Link>).
      </span>
    </>
  )
}

export default SignupPage
