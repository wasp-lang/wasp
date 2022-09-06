import React, { useState } from 'react'
import { useHistory } from 'react-router-dom'

import login from '@wasp/auth/login'
import signup from '@wasp/auth/signup'
import addWaspSourceHeader from '../addWaspSourceHeader'

import { errorMessage } from '../utils.js'

const SignupPage = () => {
  const history = useHistory()
  const [username, setUsername] = useState()
  const [email, setEmail] = useState()
  const [password, setPassword] = useState()
  const [submitError, setSubmitError] = useState()

  // TODO: Do validation in form and show validation errors.

  const handleSubmit = async (event) => {
    event.preventDefault()
    setSubmitError(null)
    try {
      await signup({ username, email, password })
      await login(username, password)
      history.push('/')
    } catch (err) {
      setSubmitError(err)
    }
  }

  // TODO: I should look into using bootstrap v4, it might make all this simpler.
  return (
    <div>
      { submitError && (
          <p> { submitError.message ? errorMessage(submitError) : submitError } </p>
      ) }

      <form onSubmit={handleSubmit}>
        <h2> Username </h2>
        <input type='text' value={username} onChange={e => setUsername(e.target.value)} />

        <h2> Email </h2>
        <input type='text' value={email} onChange={e => setEmail(e.target.value)} />

        <h2> Password </h2>
        <input type='password' value={password} onChange={e => setPassword(e.target.value)} />

        <br />
        <input type='submit' value='Sign up' />
      </form>
    </div>
  )
}

export default addWaspSourceHeader(SignupPage)
