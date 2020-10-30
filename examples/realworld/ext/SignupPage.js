import React, { useState } from 'react'
import { useHistory } from 'react-router-dom'

import signup from '@wasp/actions/signup'

export default () => {
  const history = useHistory()
  const [username, setUsername] = useState()
  const [email, setEmail] = useState()
  const [password, setPassword] = useState()

  const handleSubmit = async (event) => {
    event.preventDefault()
    try {
      await signup({ username, email, password })
      history.push('/')
      // TODO: What to do after sign up?
    } catch (err) {
      // TODO: How should we handle errors?
      window.alert('Error:' + err.message)
    }
  }

  // TODO: I should look into using bootstrap v4, it might make all this simpler.
  return (
    <div>
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
