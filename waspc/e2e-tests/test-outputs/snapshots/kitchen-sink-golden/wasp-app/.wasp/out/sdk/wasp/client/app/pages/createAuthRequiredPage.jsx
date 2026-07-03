import * as React from 'react'

import { saveOriginalRoute } from '@wasp.sh/lib-auth/browser'
import { Navigate, useLocation } from 'react-router'
import { useAuth } from '../../auth'

import { Loader } from '../components/Loader'
import { MessageError } from '../components/Message'
import { FullPageWrapper } from '../components/FullPageWrapper'

export const createAuthRequiredPage = (Page) => {
  return (props) => {
    const { data: user, status, error } = useAuth()
    const location = useLocation()

    const isRedirectingToLogin = status === 'success' && !user
    // Saved in an effect (not during render) so renders that React discards
    // can't capture a route the user never got redirected from.
    React.useEffect(() => {
      if (isRedirectingToLogin) {
        saveOriginalRoute(location.pathname + location.search + location.hash)
      }
    }, [isRedirectingToLogin, location])

    switch (status) {
      case 'success':
        if (user) {
          return <Page {...props} user={user} />
        } else {
          return <Navigate to="/login" replace />
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
