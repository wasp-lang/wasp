import { LoginForm } from "wasp/client/auth";
import React from 'react'

import { Link } from 'react-router-dom'
import addWaspSourceHeader from './addWaspSourceHeader'

const LoginPage = (props) => {
  return (
    <>
      <div style={{maxWidth: "400px", margin: "0 auto"}}>
        <LoginForm/>
        <br/>
        <span>
          I don't have an account yet (<Link to="/signup">go to signup</Link>).
        </span>
      </div>
    </>
  )
}

export default addWaspSourceHeader(LoginPage)
