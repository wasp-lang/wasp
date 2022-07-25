{{={= =}=}}
import React, { useState } from 'react'
import { useNavigate } from 'react-router-dom'

import login from '../login.js'
import { errorMessage } from '../../utils.js'

const LoginForm = () => {
  const navigate = useNavigate()

  const [emailFieldVal, setEmailFieldVal] = useState('')
  const [passwordFieldVal, setPasswordFieldVal] = useState('')

  const handleLogin = async (event) => {
    event.preventDefault()
    try {
      await login(emailFieldVal, passwordFieldVal)
      // Redirect to configured page, defaults to /.
      navigate('{= onAuthSucceededRedirectTo =}')
    } catch (err) {
      console.log(err)
      window.alert(errorMessage(err))
    }
  }
  
  return (
    <form onSubmit={handleLogin}>
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
        <input type="submit" value="Log in"/>
      </div>
    </form>
  )
}

export default LoginForm
