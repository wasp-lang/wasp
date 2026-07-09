{{={= =}=}}
import * as React from 'react'

{=^ onAuthFailedRedirectTo.isApi =}
import { Navigate } from 'react-router'
{=/ onAuthFailedRedirectTo.isApi =}
{=# onAuthFailedRedirectTo.isApi =}
import { config } from '../../config'
{=/ onAuthFailedRedirectTo.isApi =}
import { useAuth } from '../../auth'

import { Loader } from '../components/Loader'
import { MessageError } from '../components/Message'
import { FullPageWrapper } from '../components/FullPageWrapper'

export const createAuthRequiredPage = (Page) => {
  return (props) => {
    const { data: user, status, error } = useAuth()

    switch (status) {
      case 'success':
        if (user) {
          return <Page {...props} user={user} />
        } else {
          {=^ onAuthFailedRedirectTo.isApi =}
          return <Navigate to="{= onAuthFailedRedirectTo.path =}" replace />
          {=/ onAuthFailedRedirectTo.isApi =}
          {=# onAuthFailedRedirectTo.isApi =}
          return <NavigateToApiEndpoint />
          {=/ onAuthFailedRedirectTo.isApi =}
        }
      case 'loading':
        return (
          <FullPageWrapper className="wasp-auth-required-loader-wrapper">
            <Loader />
          </FullPageWrapper>
        )
      case 'error':
        return (
          <FullPageWrapper className="wasp-auth-required-error-wrapper">
            <MessageError subtitle={<small>Details: {error.message}</small>}>
              Failed to load user data. Try refreshing the page.
            </MessageError>
          </FullPageWrapper>
        )
    }
  }
}
{=# onAuthFailedRedirectTo.isApi =}

const NavigateToApiEndpoint = () => {
  React.useEffect(() => {
    window.location.replace(`${config.apiUrl}{= onAuthFailedRedirectTo.path =}`)
  }, [])

  return (
    <FullPageWrapper className="wasp-auth-required-loader-wrapper">
      <Loader />
    </FullPageWrapper>
  )
}
{=/ onAuthFailedRedirectTo.isApi =}
