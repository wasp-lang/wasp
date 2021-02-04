import React from 'react'
import Button from '@material-ui/core/Button'

import { Link } from 'react-router-dom'
import useAuth from '@wasp/auth/useAuth.js'
import logout from '@wasp/auth/logout.js'
import Todo from '../Todo.js'
import '../Main.css'

const Main = ({ user }) => {
  return (
    <>
      <Button
        variant='contained' color='primary'
        onClick={logout}>
        Logout
      </Button>
      <Todo />
    </>
  )
}

export default Main
