import React from 'react'
import Button from '@mui/material/Button'

import { Logout } from '@wasp/auth/logout.js'
import Todo from '../Todo.js'
import '../Main.css'

const Main = () => {
  return (
    <>
      <Logout>
        <Button variant='contained' color='primary'>
          Logout
        </Button>
      </Logout>
      <Todo />
    </>
  )
}

export default Main
