import React from 'react'
import { Link } from 'react-router-dom'

export const ProfilePage = ({ user }) => {
  return (
    <>
    <div>I am Profile page for { user.email }!</div>
    <br />
      <Link to='/'>Go to dashboard</Link>
    </>
  )
}
