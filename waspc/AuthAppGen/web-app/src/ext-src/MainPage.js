import React, { useState } from 'react'

import { useQuery } from '../queries'
import getUsers from '../queries/getUsers.js'
import createUser from '../actions/createUser.js'
// Added by Matija
import login from '../auth/login.js'
import logout from '../auth/logout.js'
import useMe from '../auth/me.js'
import { clearAuthToken } from '../api.js'

const MainPage = () => {

  const { me, refetchMe } = useMe()
  console.log('MainPage useMe: ', me)

  // Signup form values
  const [emailFieldVal, setEmailFieldVal] = useState('')
  const [passwordFieldVal, setPasswordFieldVal] = useState('')

  const { data: users, refetch, isFetching, isError, error: usersError} = useQuery(getUsers)

  const handleSignup = async (event) => {
    event.preventDefault()
    try {
      await createUser({ email: emailFieldVal, password: passwordFieldVal })
      setEmailFieldVal('')
      setPasswordFieldVal('')
      
      refetch()
    } catch (err) {
      console.log(err)
      window.alert('Error:' + err.message)
    }
  }

  const Users = (props) => {
    return <div>
      { props.users.map((user, idx) => <User user={user} key={idx}/>) }
    </div>
  }

  const User = (props) => {
    return <div>
      {props.user.email} {props.user.password}
    </div>
  }

  const fetchUsers = async () => {
    try {
      await getUsers()
    } catch (err) {
      console.log(err)
      window.alert('Error:' + err.message)
    }

  }

  const LoginForm = () => {
    const [loginEmail, setLoginEmail] = useState('')
    const [loginPassword, setLoginPassword] = useState('')

    const handleLogin = async (event) => {
      event.preventDefault()

      try {
        await login(loginEmail, loginPassword)

        setLoginEmail('')
        setLoginPassword('')
      } catch (err) {
        console.log(err)
        window.alert('Error:' + err.message)
      }
    }

    return (
      <form onSubmit={handleLogin}>
        <h2>Email</h2>
        <input
          type="text"
          value={loginEmail}
          onChange={e => setLoginEmail(e.target.value)}
        />
        <h2>Password</h2>
        <input
          type="password"
          value={loginPassword}
          onChange={e => setLoginPassword(e.target.value)}
        />
        <div>
          <input type="submit" value="Log in"/>
        </div>
      </form>
    )
  }

  return (
    <div>
      { me ? (
        <span>You are logged in as {me.email}.</span>
      ) : (
        <span>You are not logged in.</span>
      )}

      <h1>Sign up</h1>

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

      <h1>Login</h1>
      <LoginForm/>
      <button onClick={logout}>Logout</button>

      <h1>Users</h1>
      <button onClick={fetchUsers}>Non-reactive fetch users</button>
      <button onClick={refetch}>Refetch</button>

      { isError && <span>Error while fetching users.</span>}

      { isFetching ? (
        <div>Fetching users...</div>
      ) : (
        users && <Users users={users} />
      )}
    </div>
  )


}

export default MainPage
