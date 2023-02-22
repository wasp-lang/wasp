import React from 'react'
import { Link } from 'react-router-dom'

import SignupForm from '@wasp/auth/forms/Signup'
import addWaspSourceHeader from './addWaspSourceHeader'

const SignupPage = (props) => {
  return (
    <>
      <SignupForm/>
      <br/>
      <span>
        I already have an account (<Link to="/login">go to login</Link>).
      </span>
    </>
  )
}

export default addWaspSourceHeader(SignupPage)
