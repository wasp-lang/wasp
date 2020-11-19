import React, { useState } from 'react'
import { Link, useHistory } from 'react-router-dom'

import useAuth from '@wasp/auth/useAuth.js'
import logout from '@wasp/auth/logout.js'
import updateUser from '@wasp/actions/updateUser'

const UserSettingsPage = () => {
  const { data: user, isError } = useAuth({ keepPreviousData: true })
  // TODO: Instead of this logic here, I wish I could use ACL via Wasp and just
  //   receive user via props instead of useAuth().
  if (!user || isError) {
    return <span> Please <Link to='/login'>log in</Link>. </span>
  }

  // TODO: There is no navbar right now, but there should be one! It is only on Main page.
  //   I wish I had a "layout" mechanism that I would apply on certain pages
  //   and they would all have this same layout.
  //   For now, I should probably implement a HOC that will take care of this.

  return (
    <div>
      <UserSettings user={user}/>
    </div>
  )

}

const UserSettings = (props) => {
  const user = props.user

  const history = useHistory()

  const [profilePictureUrl, setProfilePictureUrl] = useState(user.profilePictureUrl || '')
  const [username, setUsername] = useState(user.username || '')
  const [bio, setBio] = useState(user.bio || '')
  const [email, setEmail] = useState(user.email || '')
  const [newPassword, setNewPassword] = useState('')

  const [submitError, setSubmitError] = useState(null)

  const handleSubmit = async (event) => {
    event.preventDefault()
    setSubmitError(null)
    try {
      await updateUser({
        profilePictureUrl,
        username,
        bio,
        email,
        newPassword
      })
      // TODO: If update was successful, send user to his profile (/@<username>).
      history.push(`/@${username}`)
    } catch (err) {
      setSubmitError(err)
    }
  }

  const handleLogout = async () => {
    await logout()
    history.push('/')
  }

  return (
    <div>
      { submitError && (
        <p>
          { submitError.message || submitError }
        </p>
      ) }

      <form onSubmit={handleSubmit}>

        <h2>URL of profile picture</h2>
        <input
          type='text'
          value={profilePictureUrl}
          onChange={e => setProfilePictureUrl(e.target.value)}
        />

        <h2>Username</h2>
        <input
          type='text'
          value={username}
          onChange={e => setUsername(e.target.value)}
        />

        <h2>Short bio</h2>
        <input
          type='text'
          value={bio}
          onChange={e => setBio(e.target.value)}
        />

        <h2>Email</h2>
        <input
          type='text'
          value={email}
          onChange={e => setEmail(e.target.value)}
        />

        <h2>New password</h2>
        <input
          type='password'
          value={newPassword}
          onChange={e => setNewPassword(e.target.value)}
        />

        <div>
          <input type='submit' value='Update Settings' />
        </div>
      </form>

      <button onClick={handleLogout}> Log out </button>
    </div>
  )
}

export default UserSettingsPage
