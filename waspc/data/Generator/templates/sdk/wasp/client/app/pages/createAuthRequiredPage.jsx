{{={= =}=}}
import * as React from 'react'

import { Navigate } from 'react-router'
import { useAuth } from '../../auth'

import { Loader } from '../components/Loader'
import { MessageError } from '../components/Message'
import { FullPageWrapper } from '../components/FullPageWrapper'

/**
 * The auth gate. An `authRequired` page renders only for a logged-in user;
 * without one it redirects to the login page.
 *
 * With `options.schemes`, the page additionally requires the request to have
 * been authenticated by one of the listed schemes. A logged-in user from
 * another scheme gets an access-denied message, NOT a redirect (redirecting a
 * logged-in user to the login page would loop). Page-level checks are UX; the
 * real gate is on the operations the page calls.
 */
export const createAuthRequiredPage = (Page, options) => {
  const requiredSchemes = options?.schemes ?? null

  return (props) => {
    const { data: user, status, error } = useAuth()

    switch (status) {
      case 'success':
        if (user) {
          if (requiredSchemes !== null && !requiredSchemes.includes(user.credentialScheme)) {
            return (
              <FullPageWrapper className="wasp-auth-required-forbidden-wrapper">
                <MessageError
                  subtitle={
                    <small>
                      You are signed in via '{user.credentialScheme}', but this page requires
                      signing in via {requiredSchemes.map((name) => `'${name}'`).join(' or ')}.
                    </small>
                  }
                >
                  You don't have access to this page.
                </MessageError>
              </FullPageWrapper>
            )
          }
          return <Page {...props} user={user} />
        } else {
          return <Navigate to="{= onAuthFailedRedirectTo =}" replace />
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
