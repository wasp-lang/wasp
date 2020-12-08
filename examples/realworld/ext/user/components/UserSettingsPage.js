import React, { useState } from 'react'
import { Link, useHistory } from 'react-router-dom'

import logout from '@wasp/auth/logout.js'

import updateUser from '@wasp/actions/updateUser'

import Navbar from '../../Navbar'

const UserSettingsPage = ({ user }) => {
  return (
    <div>
      <Navbar />

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
        <textarea
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
