import React, { useEffect } from 'react'
import { Link } from '@wasp/router'
import { User } from '@wasp/auth/types'
import api from '@wasp/api'
import { getName, getProviderData } from '../user'

async function fetchCustomRoute() {
  const res = await api.get('/foo/bar')
  console.log(res.data)
}

export const ProfilePage = ({ user }: { user: User }) => {
  useEffect(() => {
    fetchCustomRoute()
  }, [])

  const name = getName(user)
  const providerData = getProviderData(user)

  return (
    <>
      <h2>Profile page</h2>
      <div>
        Hello <strong>{name}</strong>! Your status is{' '}
        <strong>
          {providerData && providerData.isEmailVerified
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
