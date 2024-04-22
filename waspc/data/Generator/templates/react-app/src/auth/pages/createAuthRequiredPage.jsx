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
      // TODO: User ErrorBoundary instead (therefore I will likely want to throw error here).
      return <span>Error occurred, try reloading the page. {error?.message}</span>
    }
  }
}

export default createAuthRequiredPage
