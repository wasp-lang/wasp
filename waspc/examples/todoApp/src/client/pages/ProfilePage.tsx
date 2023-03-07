import React, { useEffect } from 'react'
import { Link } from 'react-router-dom'
import { User } from '@wasp/entities'
import api from '@wasp/api'

async function logCustomRoute() {
  const res = await api.get('/foo/bar')
  console.log(res.data)
}

export const ProfilePage = ({ user: { username } }: { user: User }) => {
  useEffect(() => {
    logCustomRoute()
  }, []);

  return (
    <>
      <h2>Profile page</h2>
      <div>
        Hello <strong>{username}</strong>!
      </div>
      <br />
      <Link to="/">Go to dashboard</Link>
    </>
  )
}
