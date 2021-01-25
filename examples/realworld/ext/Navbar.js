import React from 'react'
import { Link } from 'react-router-dom'

import useAuth from '@wasp/auth/useAuth.js'


const Navbar = () => {
  const { data: user } = useAuth()

  if (user) {
    return (
      <div>
        <Link to='/'> Home </Link>
        <Link to='/editor'> New Article </Link>
        <Link to='/settings'> Settings </Link>
        <Link to={`/@${user.username}`}> { user.username } </Link>
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

export default Navbar
