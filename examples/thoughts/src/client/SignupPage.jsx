import { SignupForm } from "wasp/client/auth";
import React from 'react'

import { Link } from 'react-router-dom'
import addWaspSourceHeader from './addWaspSourceHeader'

const SignupPage = (props) => {
  return (
    <>
      <div style={{maxWidth: "400px", margin: "0 auto"}}>
        <SignupForm/>
        <br/>
        <span>
          I already have an account (<Link to="/login">go to login</Link>).
        </span>
      </div>
    </>
  )
}

export default addWaspSourceHeader(SignupPage)
