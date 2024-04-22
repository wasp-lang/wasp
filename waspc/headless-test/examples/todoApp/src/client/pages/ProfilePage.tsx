import React, { useEffect } from 'react'
import { type AuthUser as User } from 'wasp/auth'
import { Link } from 'wasp/client/router'
import { api } from 'wasp/client/api'
import { getName } from '../user'

async function fetchCustomRoute() {
  const res = await api.get('/foo/bar')
  console.log(res.data)
}

export const ProfilePage = ({ user }: { user: User }) => {
  useEffect(() => {
    fetchCustomRoute()
  }, [])

  const name = getName(user)

  return (
    <>
      <h2>Profile page</h2>
      <div>
        Hello <strong>{name}</strong>! Your status is{' '}
        <strong>
          {user.identities.email && user.identities.email.isEmailVerified
            ? 'verfied'
            : 'unverified'}
        </strong>
        .
      </div>
      <br />
      <Link to="/">Go to dashboard</Link>
    </>
  )
}
