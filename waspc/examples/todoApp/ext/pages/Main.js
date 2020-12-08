import React from 'react'

import { Link } from 'react-router-dom'
import useAuth from '@wasp/auth/useAuth.js'
import logout from '@wasp/auth/logout.js'
import Todo from '../Todo.js'
import '../Main.css'

const Main = ({ user }) => {
  return (
    <>
      <button onClick={logout}>Logout</button>
      <Todo />
    </>
  )
}

export default Main
