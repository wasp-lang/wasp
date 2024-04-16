{{={= =}=}}
import React from 'react'

import { Redirect } from 'react-router-dom'
import { useAuth } from 'wasp/client/auth'

{=# pageLoader.isDefined =}
{=& pageLoader.importStatement =}
// TODO: Do we have a way here to simply provide an alias?
//   Then I could set alias to PageLoader and use PageLoader below also and all great.
//   But now I can't because user might call their compnent PageLoader and then I get a conflict.
const Wasp_PageLoader = {=& pageLoader.importIdentifier =}
{=/ pageLoader.isDefined =}
{=^ pageLoader.isDefined =}
import { DefaultPageLoader as Wasp_PageLoader } from './DefaultPageLoader.jsx';
{=/ pageLoader.isDefined =}

const createAuthRequiredPage = (Page) => {
  return (props) => {
    const { data: user, status, error } = useAuth();

    if (status === 'success') {
      if (user) {
        return (
          <Page {...props} user={user} />
        );
      } else {
        return <Redirect to="{= onAuthFailedRedirectTo =}" />;
      }
    } else {
      return <Wasp_PageLoader status={status} error={error} />;
    }
  }
}

export default createAuthRequiredPage

