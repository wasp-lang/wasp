{{={= =}=}}
import React from 'react'

import { Redirect } from 'react-router-dom'
import { useAuth } from 'wasp/client/auth'

import { DefaultLoader } from '../../DefaultLoader'

const createAuthRequiredPage = (Page) => {
  return (props) => {
    const { data: user, status, error } = useAuth();

    switch (status) {
      case 'success':
        if (user) {
          return (
            <Page {...props} user={user} />
          );
        } else {
          return <Redirect to="{= onAuthFailedRedirectTo =}" />;
        }
      case 'loading':
        return <DefaultLoader/>;
    case 'error':
      return <span>An error occurred. Please refresh the page. {error?.message}</span>
    }
  }
}

export default createAuthRequiredPage
