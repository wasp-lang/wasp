import React, { useState } from 'react'

import { Link } from "react-router-dom"
import useAuth from '@wasp/auth/useAuth.js'
import logout from '@wasp/auth/logout.js'
import Todo from "../Todo.js"

import '../Main.css'

const Main = () => {
  const { data: user } = useAuth()

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
