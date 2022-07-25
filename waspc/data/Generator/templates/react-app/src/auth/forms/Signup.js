{{={= =}=}}
import React, { useState } from 'react'
import { useNavigate } from 'react-router-dom'

import signup from '../signup.js'
import login from '../login.js'
import { errorMessage } from '../../utils.js'

const SignupForm = () => {
  const navigate = useNavigate()

  const [emailFieldVal, setEmailFieldVal] = useState('')
  const [passwordFieldVal, setPasswordFieldVal] = useState('')

  const handleSignup = async (event) => {
    event.preventDefault()
    try {
      await signup({ email: emailFieldVal, password: passwordFieldVal })
      await login (emailFieldVal, passwordFieldVal)

      setEmailFieldVal('')
      setPasswordFieldVal('')

      // Redirect to configured page, defaults to /.
      navigate('{= onAuthSucceededRedirectTo =}')
    } catch (err) {
      console.log(err)
      window.alert(errorMessage(err))
    }
  }
  
  return (
    <form onSubmit={handleSignup}>
      <h2>Email</h2>
      <input
        type="text"
        value={emailFieldVal}
        onChange={e => setEmailFieldVal(e.target.value)}
      />
      <h2>Password</h2>
      <input
        type="password"
        value={passwordFieldVal}
        onChange={e => setPasswordFieldVal(e.target.value)}
      />
      <div>
        <input type="submit" value="Sign up"/>
      </div>
    </form>
  )
}

export default SignupForm
