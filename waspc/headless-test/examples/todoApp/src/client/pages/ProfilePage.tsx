import React, { useEffect } from 'react'
import { Link } from '@wasp/router'
import { User } from '@wasp/entities'
import api from '@wasp/api'

async function fetchCustomRoute() {
  const res = await api.get('/foo/bar')
  console.log(res.data)
}

export const ProfilePage = ({ user }: { user: any }) => {
  useEffect(() => {
    fetchCustomRoute()
  }, [])

  const email = user.auth.identities.find(
    (i: any) => i.providerName === 'email'
  )?.providerUserId
  const isEmailVerified = JSON.parse(
    user.auth.identities.find((i: any) => i.providerName === 'email')
      ?.providerData
  ).isEmailVerified

  console.log(user.auth.identities)

  return (
    <>
      <h2>Profile page</h2>
      <div>
        Hello <strong>{email}</strong>! Your status is{' '}
        <strong>{isEmailVerified ? 'verfied' : 'unverified'}</strong>.
      </div>
      <br />
      <Link to="/">Go to dashboard</Link>
    </>
  )
}
