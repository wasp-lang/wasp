import React, { useState } from 'react'
import { Link, useHistory } from 'react-router-dom'

import useAuth from '@wasp/auth/useAuth.js'
import logout from '@wasp/auth/logout.js'
import updateUser from '@wasp/actions/updateUser'
import getUser from '@wasp/queries/getUser'
import { useQuery } from '@wasp/queries'

const UserProfilePage = (props) => {
  const history = useHistory()

  const username = props.match.params.username
  const { data: user, error: userError } = useQuery(getUser, { username })

  console.log(user, userError)
  if (userError) {
    console.log(userError)
    history.push("/")
  }

  // TODO: There is no navbar right now, but there should be one! It is only on Main page.
  //   I wish I had a "layout" mechanism that I would apply on certain pages
  //   and they would all have this same layout.
  //   For now, I should probably implement a HOC that will take care of this.

  return user ? (
    <div>
      <p> { user.username } </p>
      <p> { user.bio } </p>
      <div>
        <Link to='/settings'>Edit Profile Settings</Link>
      </div>
    </div>
  ) : null
}

export default UserProfilePage
