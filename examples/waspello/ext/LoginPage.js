import React from 'react'
import { Link } from 'react-router-dom'

import LoginForm from '@wasp/auth/forms/Login'
import addWaspSourceHeader from './addWaspSourceHeader.js'

import mainLogo from './waspello-logo.svg'
import './Signup.css'

const LoginPage = (props) => {
  return (
    <div className="auth-root-container">

      <img alt="Waspello" className="main-logo" src={mainLogo} />

      <div className="auth-form-container">
        <LoginForm/>
        <p>
          I don't have an account yet (<Link to="/signup">go to signup</Link>).
        </p>
      </div>

    </div>
  )
}

export default addWaspSourceHeader(LoginPage)
