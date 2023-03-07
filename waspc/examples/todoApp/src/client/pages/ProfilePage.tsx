import React, { useEffect } from 'react'
import { Link } from 'react-router-dom'
import { User } from '@wasp/entities'
import api from '@wasp/api'

export const ProfilePage = ({ user: { username } }: { user: User }) => {
  useEffect(() => {
    const fetchData = async () => {
      const res = await api.get('/foo/bar')
      return res.data
    }
    fetchData().then(data => console.log(data)).catch(console.error);
  }, [])

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
