import React from 'react'
import { Link } from 'react-router-dom'

import useAuth from '@wasp/auth/useAuth.js'
import logout from '@wasp/auth/logout.js'

const MainPage = () => {
  const { data: user } = useAuth()

  return (
    <div>
      <UserWidget user={user} />
    </div>
  )
}

const UserWidget = (props) => {
  if (props.user) {
    // TODO: Make links lead somewhere.
    return (
      <div>
        <a href="#"> Settings </a>
        <a href="#"> { props.user.username } </a>
        <button onClick={logout}> Log out </button>
      </div>
    )
  } else {
    return (
      <div>
        <Link to='/login'> Sign in </Link>
        <Link to='/register'> Sign up </Link>
      </div>
    )
  }
}

export default MainPage
