import React, { useState } from 'react'
import { Link, useHistory } from 'react-router-dom'

import signup from '@wasp/auth/signup'
import login from '@wasp/auth/login'

import EmailAndPassForm from './Auth/EmailAndPassForm'
import GoogleAuthButton from './Auth/GoogleAuthButton'
import addWaspSourceHeader from './addWaspSourceHeader'

import mainLogo from './waspello-logo.svg'
import './Signup.css'


const SignupPage = (props) => {

  const history = useHistory()

  const [usernameFieldVal, setUsernameFieldVal] = useState('')
  const [passwordFieldVal, setPasswordFieldVal] = useState('')

  const handleSignup = async (event) => {
    event.preventDefault()
    try {
      await signup({ username: usernameFieldVal, password: passwordFieldVal })
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
          title='Sign up for your account'
          userField={usernameFieldVal}
          passField={passwordFieldVal}
          setUser={setUsernameFieldVal}
          setPass={setPasswordFieldVal}
          handleSignup={handleSignup}
        />

        <div className='mt-3 text-xs text-neutral-500'>
          OR
        </div>

        <GoogleAuthButton />

        <div className='w-full text-center mt-6 pt-3 border-t border-neutral-300'>
          <p className='text-sm text-yellow-600'>
            <Link to='/login'>
              Already have an Waspello account? Log in.
            </Link>
          </p>
        </div>
      </div>

    </div>
  )
}


export default addWaspSourceHeader(SignupPage)
