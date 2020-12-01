import React, { useState } from 'react'
import { useHistory } from 'react-router-dom'

import login from '@wasp/auth/login.js'

// TODO: A lot of duplication with the Sign up page, extract it into one component
//   and then just use it in both LoginPage and SignupPage?

const LoginPage = () => {
  const history = useHistory()
  const [email, setEmail] = useState()
  const [password, setPassword] = useState()
  const [submitError, setSubmitError] = useState()

  const handleSubmit = async (event) => {
    event.preventDefault()
    setSubmitError(null)
    try {
      await login(email, password)
      history.push('/')
    } catch (err) {
      // TODO: If error is 401, inform user that either username or password is not right.
      setSubmitError(err)
    }
  }

  // TODO: I should look into using bootstrap v4, it might make all this simpler.
  return (
    <div>
      { submitError && (
          <p> { submitError.message || submitError } </p>
      ) }

      <form onSubmit={handleSubmit}>
        <h2> Email </h2>
        <input type='text' value={email} onChange={e => setEmail(e.target.value)} />

        <h2> Password </h2>
        <input type='password' value={password} onChange={e => setPassword(e.target.value)} />

        <br />
        <input type='submit' value='Sign in' />
      </form>
    </div>
  )
}

export default LoginPage
