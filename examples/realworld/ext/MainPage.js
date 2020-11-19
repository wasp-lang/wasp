import React from 'react'
import { Link } from 'react-router-dom'

import useAuth from '@wasp/auth/useAuth.js'

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
        <Link to='/settings'> Settings </Link>
        <a href={`/@${props.user.username}`}> { props.user.username } </a>
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
