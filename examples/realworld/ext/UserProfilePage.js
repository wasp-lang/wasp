import React, { useState } from 'react'
import { Link, useHistory } from 'react-router-dom'

import useAuth from '@wasp/auth/useAuth.js'
import logout from '@wasp/auth/logout.js'
import updateUser from '@wasp/actions/updateUser'
import getUser from '@wasp/queries/getUser'
import { useQuery } from '@wasp/queries'

import Navbar from './Navbar'
import smileyImageUrl from './smiley.jpg'

const UserProfilePage = (props) => {
  const history = useHistory()

  const username = props.match.params.username
  const { data: user, error: userError } = useQuery(getUser, { username })

  // NOTE: use-query will retry multiple times in case of error, making user
  //   wait relatively long before giving up on for example 404 and returning error,
  //   while on the other hand we would probably like it to give up on 404 immediatelly!
  //   Should we modify react-query default settings to not do any retrying?
  if (userError) {
    console.log(userError)
    history.push("/")
  }

  // TODO: List My Articles
  // TODO: List Favorited Articles

  return user ? (
    <div>
      <Navbar />

      <img src={user.profilePictureUrl || smileyImageUrl} />
      <p> { user.username } </p>
      <p> { user.bio } </p>
      <div>
        <Link to='/settings'>Edit Profile Settings</Link>
      </div>
    </div>
  ) : null
}

export default UserProfilePage
