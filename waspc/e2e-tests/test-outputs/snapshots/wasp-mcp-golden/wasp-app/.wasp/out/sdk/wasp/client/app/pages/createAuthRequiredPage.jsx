import * as React from 'react'

import { Navigate, useLocation } from 'react-router'
import { useAuth } from '../../auth'
import { rememberMcpReturnTo } from '../../mcp/returnTo'

import {
  FullPageWrapper,
  Loader,
  MessageError,
} from '@wasp/lib-sdk-core/browser'

export const createAuthRequiredPage = (Page) => {
  return (props) => {
    const { data: user, status, error } = useAuth()
    const location = useLocation()

    switch (status) {
      case 'success':
        if (user) {
          return <Page {...props} user={user} />
        } else {
          // The MCP consent page needs its full URL back after login.
          rememberMcpReturnTo(location)
          return <Navigate to={"/login"} replace />
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
