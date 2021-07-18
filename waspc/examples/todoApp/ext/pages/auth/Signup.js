import React, { useState } from 'react'
import { Link } from 'react-router-dom'

import SignupForm from '@wasp/auth/forms/Signup'
import getNumTasks from '@wasp/queries/getNumTasks'
import { useQuery } from '@wasp/queries'


const Signup = (props) => {
  const { data: numTasks, isError, error: tasksError } = useQuery(getNumTasks)
  return (
    <>
      <SignupForm/>
      <br/>
      <span>
        I already have an account (<Link to="/login">go to login</Link>).
      </span>
      <br/>
      <span>
        Number of tasks already created: {numTasks}
      </span>
    </>
  )
}

export default Signup
