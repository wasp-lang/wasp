{{={= =}=}}
import * as React from 'react'

{=# isMcpEnabled =}
import { Navigate, useLocation } from 'react-router'
{=/ isMcpEnabled =}
{=^ isMcpEnabled =}
import { Navigate } from 'react-router'
{=/ isMcpEnabled =}
import { useAuth } from '../../auth'
{=# isMcpEnabled =}
import { rememberMcpReturnTo } from '../../mcp/returnTo'
{=/ isMcpEnabled =}

import {
  FullPageWrapper,
  Loader,
  MessageError,
} from '@wasp/lib-sdk-core/browser'

export const createAuthRequiredPage = (Page) => {
  return (props) => {
    const { data: user, status, error } = useAuth()
    {=# isMcpEnabled =}
    const location = useLocation()
    {=/ isMcpEnabled =}

    switch (status) {
      case 'success':
        if (user) {
          return <Page {...props} user={user} />
        } else {
          {=# isMcpEnabled =}
          // The MCP consent page needs its full URL back after login.
          rememberMcpReturnTo(location)
          {=/ isMcpEnabled =}
          return <Navigate to={{=& onAuthFailedRedirectTo =}} replace />
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
