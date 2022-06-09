import React from 'react'
import { useHistory } from 'react-router-dom'
import Button from '@material-ui/core/Button'

import logout from '@wasp/auth/logout.js'
import Todo from '../Todo.js'
import '../Main.css'

const Main = () => {
  const history = useHistory()

  // TODO: Make this a proper component we provide.
  const handleLogout = async () => {
    try {
      console.log("called handleLogout")
      await logout()
      history.push('/login')
    } catch (err) {
      console.log(err)
    }
  }

  return (
    <>
      <Button
        variant='contained' color='primary'
        onClick={handleLogout}>
        Logout
      </Button>
      <Todo />
    </>
  )
}

export default Main
