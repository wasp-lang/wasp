import React, { useState } from 'react'

import { Link, useNavigate } from 'react-router-dom'

import EmailAndPassForm from './components/EmailAndPassForm'
import GoogleAuthButton from './components/GoogleAuthButton'
import addWaspSourceHeader from '../common/addWaspSourceHeader'

import mainLogo from '../common/waspello-logo.svg'
import './Signup.css'
import { login } from "wasp/client/auth";

const LoginPage = (props) => {
  const navigate = useNavigate()

  const [usernameFieldVal, setUsernameFieldVal] = useState('')
  const [passwordFieldVal, setPasswordFieldVal] = useState('')
  const [errorMessage, setErrorMessage] = useState('')

  const handleLogin = async (event) => {
    event.preventDefault()
    setErrorMessage('')

    try {
      await login(usernameFieldVal, passwordFieldVal)

      setUsernameFieldVal('')
      setPasswordFieldVal('')

      navigate('/')
    } catch (err) {
      if (err.statusCode == 422) {
        const errorMessage = err?.data?.data?.message || 'Invalid request'
        setErrorMessage(errorMessage)
      } else if (err.statusCode == 401) {
        const errorMessage = err?.data?.message || 'Unauthorized'
        setErrorMessage(errorMessage)
      } else {
        setErrorMessage('An error occurred. Please try again later.')
      }
    }
  }

  return (
    <div className="auth-root-container">
      <img alt="Waspello" className="main-logo" src={mainLogo} />

      <div className="auth-form-container">

        <EmailAndPassForm
          title='Log in with your account'
          submitButtonLabel='Log in'
          userField={usernameFieldVal}
          passField={passwordFieldVal}
          setUser={setUsernameFieldVal}
          setPass={setPasswordFieldVal}
          errorMessage={errorMessage}
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
