import React, { useState } from 'react'
import { Link, useHistory } from 'react-router-dom'

import login from '@wasp/auth/login.js'

import EmailAndPassForm from './Auth/EmailAndPassForm'
import GoogleAuthButton from './Auth/GoogleAuthButton'
import addWaspSourceHeader from './addWaspSourceHeader'

import mainLogo from './waspello-logo.svg'
import './Signup.css'

const LoginPage = (props) => {
  const history = useHistory()

  const [usernameFieldVal, setUsernameFieldVal] = useState('')
  const [passwordFieldVal, setPasswordFieldVal] = useState('')

  const handleLogin = async (event) => {
    event.preventDefault()
    try {
      await login (usernameFieldVal, passwordFieldVal)

      setUsernameFieldVal('')
      setPasswordFieldVal('')

      history.push('/')
    } catch (err) {
      console.log(err)
      window.alert(err)
    }
  }

  return (
    <div className="auth-root-container">
      <img alt="Waspello" className="main-logo" src={mainLogo} />

      <div className="auth-form-container">
        <EmailAndPassForm
          title='Log in with your account'
          userField={usernameFieldVal}
          passField={passwordFieldVal}
          setUser={setUsernameFieldVal}
          setPass={setPasswordFieldVal}
          handleSignup={handleLogin}
        />

        <div className='mt-3 text-xs text-neutral-500'>
          OR
        </div>

        <GoogleAuthButton />

        <div className='w-full text-center mt-6 pt-3 border-t border-neutral-300'>
          <p className='text-sm text-yellow-600'>
            <Link to='/signup'>
              I don't have an account yet! Sign up.
            </Link>
          </p>
        </div>
      </div>

    </div>
  )
}

export default addWaspSourceHeader(LoginPage)
