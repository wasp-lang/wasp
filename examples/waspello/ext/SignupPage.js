import React from 'react'
import { Link } from 'react-router-dom'

import SignupForm from '@wasp/auth/forms/Signup'
import addWaspSourceHeader from './addWaspSourceHeader.js'

import mainLogo from './waspello-logo.svg'
import './Signup.css'

const SignupPage = (props) => {
  return (
    <>
    <WaspSourceHeader name="Waspello" />
    <div className="auth-root-container">

      <img alt="Waspello" className="main-logo" src={mainLogo} />

      <div className="auth-form-container">
        <SignupForm/>
        <p>
          I already have an account (<Link to="/login">go to login</Link>).
        </p>
      </div>

    </div>
    </>
  )
}

export default addWaspSourceHeader(SignupPage)
