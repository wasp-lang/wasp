import React, { useState } from 'react'
import { useHistory, Link } from 'react-router-dom'

import SignupForm from '@wasp/auth/forms/Signup'

const Signup = (props) => {
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

export default Signup
