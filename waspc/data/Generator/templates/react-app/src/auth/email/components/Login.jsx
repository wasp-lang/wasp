{{={= =}=}}
import { useState } from 'react'
import { useHistory } from 'react-router-dom'

import { login } from '../actions/login';
import { errorMessage } from '../../../utils.js'

export const LoginForm = () => {
  const history = useHistory()

  const [emailFieldVal, setEmailFieldVal] = useState('')
  const [passwordFieldVal, setPasswordFieldVal] = useState('')

  const handleLogin = async (event) => {
    event.preventDefault()
    try {
      await login({
        email: emailFieldVal,
        password: passwordFieldVal,
      })
      history.push('{= onAuthSucceededRedirectTo =}')
    } catch (err) {
      console.log(err)
      window.alert(errorMessage(err))
    }
  }
  
  return (
    <form onSubmit={handleLogin} className="login-form auth-form">
      <h2>Email</h2>
      <input
        type="email"
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
