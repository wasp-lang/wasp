import React from 'react'

import { Redirect } from 'react-router-dom'
import { useAuth } from 'wasp/client/auth'

import { Loader } from '../../components/Loader'
import { MessageError } from '../../components/Message'
import { FullPageWrapper } from '../../components/FullPageWrapper'

const createAuthRequiredPage = (Page) => {
  return (props) => {
    const { data: user, status, error } = useAuth()

    switch (status) {
      case 'success':
        if (user) {
          return <Page {...props} user={user} />
        } else {
          return <Redirect to="/login" />
        }
      case 'loading':
        return (
          <FullPageWrapper className="auth-required-loader-wrapper">
            <Loader />
          </FullPageWrapper>
        )
      case 'error':
        return (
          <FullPageWrapper className="auth-required-error-wrapper">
            <MessageError subtitle={<small>Details: {error.message}</small>}>
              Failed to load user data. Try refreshing the page.
            </MessageError>
          </FullPageWrapper>
        )
    }
  }
}

export default createAuthRequiredPage
