import React, { useState } from 'react'

import { Link } from "react-router-dom"
import useUser from '@wasp/auth/useUser.js'
import logout from '@wasp/auth/logout.js'
import Todo from "../Todo.js"

import '../Main.css'

const Main = () => {
  const { user } = useUser()

  if (!user) {
    return (
      <span>
        Please <Link to="/login">login</Link> or <Link to="/signup">sign up</Link>.
      </span>
    )
  } else {
    return <>
      <button onClick={logout}>Logout</button>
      <Todo/>
    </>
  }
}

export default Main
