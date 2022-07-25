import React from 'react'
import Button from '@mui/material/Button'

import logout from '@wasp/auth/logout.js'
import Todo from '../Todo.js'
import '../Main.css'

const Main = () => {
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
