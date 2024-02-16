import React from 'react'

import { Redirect } from 'react-router-dom'
import { useAuth } from 'wasp/client/auth'


const createAuthRequiredPage = (Page) => {
  return (props) => {
    const { data: user, isError, isSuccess, isLoading } = useAuth()

    if (isSuccess) {
      if (user) {
        return (
          <Page {...props} user={user} />
        )
      } else {
        return <Redirect to="/login" />
      }
    } else if (isLoading) {
      return <span>Loading...</span>
    } else if (isError) {
      return <span>An error ocurred. Please refresh the page.</span>
    } else {
      return <span>An unknown error ocurred. Please refresh the page.</span>
    }
  }
}

export default createAuthRequiredPage

