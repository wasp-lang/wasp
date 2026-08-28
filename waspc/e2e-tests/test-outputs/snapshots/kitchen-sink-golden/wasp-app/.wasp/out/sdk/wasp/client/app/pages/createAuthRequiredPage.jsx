import * as React from 'react'

import { Navigate } from 'react-router'
import { useAuth, resumeSession } from '../../auth'

import { Loader } from '../components/Loader'
import { MessageError } from '../components/Message'
import { FullPageWrapper } from '../components/FullPageWrapper'

/**
 * The auth gate. Also the ONE place silent session resume runs (ASP.NET's
 * challenge model, in-page): when an authRequired page finds no user, the
 * provider of the last login gets one chance to re-establish the session
 * before we give up and redirect to the login page. Because resume happens
 * before any navigation, the user stays on the page they wanted -- no
 * return-url plumbing.
 *
 * With `options.providers`, the page additionally requires the session to have
 * been minted by one of the listed providers. A logged-in user from another
 * provider gets an access-denied message, NOT a redirect (redirecting a
 * logged-in user to the login page would loop). Page-level checks are UX; the
 * real gate is on the operations the page calls.
 */
export const createAuthRequiredPage = (Page, options) => {
  const requiredProviderIds = options?.providers ?? null

  return (props) => {
    const { data: user, status, error } = useAuth()
    // 'idle' -> 'attempting' -> 'settled'; resumeSession() itself is
    // single-flighted, so strict-mode double effects cannot double-exchange.
    const [resumeStatus, setResumeStatus] = React.useState('idle')

    const shouldAttemptResume = status === 'success' && !user && resumeStatus === 'idle'

    React.useEffect(() => {
      if (!shouldAttemptResume) {
        return
      }
      setResumeStatus('attempting')
      resumeSession().finally(() => setResumeStatus('settled'))
    }, [shouldAttemptResume])

    switch (status) {
      case 'success':
        if (user) {
          if (requiredProviderIds !== null && !requiredProviderIds.includes(user.sessionProviderId)) {
            return (
              <FullPageWrapper className="wasp-auth-required-forbidden-wrapper">
                <MessageError
                  subtitle={
                    <small>
                      You are signed in via '{user.sessionProviderId}', but this page requires
                      signing in via {requiredProviderIds.map((id) => `'${id}'`).join(' or ')}.
                    </small>
                  }
                >
                  You don't have access to this page.
                </MessageError>
              </FullPageWrapper>
            )
          }
          return <Page {...props} user={user} />
        } else if (resumeStatus !== 'settled') {
          // Give silent resume its chance before bouncing to the login page.
          // A successful resume refetches the user, flipping this render.
          return (
            <FullPageWrapper className="wasp-auth-required-loader-wrapper">
              <Loader />
            </FullPageWrapper>
          )
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
