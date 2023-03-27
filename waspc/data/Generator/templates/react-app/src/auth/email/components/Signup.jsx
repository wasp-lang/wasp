import { useState } from 'react'
import { useHistory } from 'react-router-dom'

import { signup } from '../actions/signup'
import { login } from '../actions/login'
import { errorMessage } from '../../../utils.js'

export const SignupForm = () => {
  const history = useHistory()

  const [emailFieldVal, setEmailFieldVal] = useState('')
  const [passwordFieldVal, setPasswordFieldVal] = useState('')

  const handleSignup = async (event) => {
    event.preventDefault()
    try {
      await signup({ email: emailFieldVal, password: passwordFieldVal })
      await login ({
        email: emailFieldVal,
        password: passwordFieldVal,
      })

      setEmailFieldVal('')
      setPasswordFieldVal('')

      history.push('/')
    } catch (err) {
      console.log(err)
      window.alert(errorMessage(err))
    }
  }
  
  return (
    <form onSubmit={handleSignup} className='signup-form auth-form'>
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
