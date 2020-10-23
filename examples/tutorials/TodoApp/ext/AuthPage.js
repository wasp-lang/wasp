import React, { useState } from 'react'
import { useHistory } from 'react-router-dom'

import signUp from '@wasp/actions/signUp.js'
import login from '@wasp/auth/login.js'

export default () => {
  const [method, setMethod] = useState('login')

  const toggleMethod = () => {
    setMethod(method === 'login' ? 'signup' : 'login')
  }

  return (
    <>
      <AuthForm method={method} />
      <a href='javascript:;' onClick={toggleMethod}>
        {method === 'login'
          ? 'I don\'t have an account yet (go to sign up).'
          : 'I already have an account (go to log in).'}
      </a>
    </>
  )
}

const AuthForm = (props) => {
  const history = useHistory()
  const [email, setEmail] = useState('')
  const [password, setPassword] = useState('')

  const handleSubmit = async (event) => {
    event.preventDefault()
    try {
      if (props.method === 'signup') {
        await signUp({ email, password })
      }
      await login(email, password)
      history.push('/')
    } catch (err) {
      window.alert('Error:' + err.message)
    }
  }

  return (
    <form onSubmit={handleSubmit}>
      <h2>Email</h2>
      <input
        type='text'
        value={email}
        onChange={e => setEmail(e.target.value)}
      />
      <h2>Password</h2>
      <input
        type='password'
        value={password}
        onChange={e => setPassword(e.target.value)}
      />
      <div>
        <input type='submit' value={props.method === 'signup' ? 'Sign up' : 'Log in'} />
      </div>
    </form>
  )
}
