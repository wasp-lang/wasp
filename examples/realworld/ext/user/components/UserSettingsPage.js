import React, { useState } from 'react'
import { Link, useHistory } from 'react-router-dom'

import Container from '@material-ui/core/Container'
import Grid from '@material-ui/core/Grid'
import TextField from '@material-ui/core/TextField'
import Button from '@material-ui/core/Button'
import { makeStyles } from '@material-ui/core/styles'

import logout from '@wasp/auth/logout.js'

import updateUser from '@wasp/actions/updateUser'

import Navbar from '../../Navbar'

const UserSettingsPage = ({ user }) => {
  return (
    <Container maxWidth="lg">
      <Navbar />
      <Grid container direction="row" justify="center">
        <Grid item xs={6}>
          <UserSettings user={user}/>
        </Grid>
      </Grid>
    </Container>
  )

}

const useStyles = makeStyles((theme) => ({
  textField: {
    //width: '25ch',
    marginBottom: theme.spacing(3)
  },
  logoutButton: {
    marginTop: theme.spacing(3)
  }
}))

const UserSettings = (props) => {
  const classes = useStyles()

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

        <TextField
          className={classes.textField}
          label="URL of profile picture"
          fullWidth
          value={profilePictureUrl} 
          onChange={e => setProfilePictureUrl(e.target.value)}
        />

        <TextField
          className={classes.textField}
          label="Username"
          fullWidth
          value={username} 
          onChange={e => setUsername(e.target.value)}
        />

        <TextField
          className={classes.textField}
          label="Short bio"
          multiline
          rows={3}
          fullWidth
          value={bio} 
          onChange={e => setBio(e.target.value)}
        />

        <TextField
          className={classes.textField}
          label="Email"
          fullWidth
          value={email} 
          onChange={e => setEmail(e.target.value)}
        />

        <TextField
          className={classes.textField}
          label="New password"
          type="password"
          fullWidth
          value={newPassword} 
          onChange={e => setNewPassword(e.target.value)}
        />

        <Button type="submit" color="primary" variant="contained">Update Settings</Button>
      </form>

      <Button
        className={classes.logoutButton}
        type="submit" color="secondary"
        variant="contained" onClick={handleLogout}
      >
        Log out
      </Button>
    </div>
  )
}

export default UserSettingsPage
