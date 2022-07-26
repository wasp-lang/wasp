{{={= =}=}}
import React from 'react'
import { useNavigate } from 'react-router-dom'

import { clearLocalStorage } from '../api.js'
import { invalidateAndRemoveQueries } from '../operations/resources'

export default async function logout() {
  clearLocalStorage()
  // TODO(filip): We are currently invalidating and removing  all the queries, but
  // we should remove only the non-public, user-dependent ones.
  await invalidateAndRemoveQueries()
}

// HOC to handle the navigate redirect. This cannot be done
// outside a React context.
export const Logout = ({ children }) => {
  const navigate = useNavigate()

  async function logoutAndNavigate() {
    await logout()
    navigate("{= onAuthFailedRedirectTo =}")
  }

  return (
    <div onClick={logoutAndNavigate}>
      {children}
    </div>
  )
}
