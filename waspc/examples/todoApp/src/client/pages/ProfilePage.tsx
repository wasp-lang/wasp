import React, { useEffect } from 'react'
import { Link } from 'react-router-dom'
import { User } from '@wasp/auth/types'
import api from '@wasp/api'

async function fetchCustomRoute() {
  const res = await api.get('/foo/bar')
  console.log(res.data)
}

export const ProfilePage = ({
  user: { email, isEmailVerified },
}: {
  user: User
}) => {
  useEffect(() => {
    fetchCustomRoute()
  }, [])

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
