import React, { useState } from 'react'
import { useHistory } from 'react-router-dom'

import signup from '../signup.js'
import login from '../login.js'
import { errorMessage } from '../../utils.js'

const SignupForm = () => {
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

      // Redirect to configured page, defaults to /.
      history.push('/')
    } catch (err) {
      console.log(err)
      window.alert(errorMessage(err))
    }
  }
  
  return (
    <form onSubmit={handleSignup} className='signup-form auth-form'>
      <h2>Username</h2>
      <input
        type="text"
        value={usernameFieldVal}
        onChange={e => setUsernameFieldVal(e.target.value)}
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
