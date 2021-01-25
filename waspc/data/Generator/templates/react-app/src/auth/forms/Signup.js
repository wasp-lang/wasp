import React, { useState } from 'react'
import { useHistory } from 'react-router-dom'

import signup from '../signup.js'
import login from '../login.js'

const SignupForm = () => {
  const history = useHistory()

  const [emailFieldVal, setEmailFieldVal] = useState('')
  const [passwordFieldVal, setPasswordFieldVal] = useState('')

  const handleSignup = async (event) => {
    event.preventDefault()
    try {
      await signup({ email: emailFieldVal, password: passwordFieldVal })
      await login (emailFieldVal, passwordFieldVal)

      setEmailFieldVal('')
      setPasswordFieldVal('')

      // Redirect to main page.
      history.push('/')
    } catch (err) {
      console.log(err)
      window.alert('Error:' + err.message)
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
