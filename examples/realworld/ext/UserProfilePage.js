import React, { useState } from 'react'
import { Link, useHistory } from 'react-router-dom'

import useAuth from '@wasp/auth/useAuth.js'
import getUser from '@wasp/queries/getUser'
import getArticlesByUser from '@wasp/queries/getArticlesByUser'
import getFavoritedArticles from '@wasp/queries/getFavoritedArticles'
import followUser from '@wasp/actions/followUser'
import { useQuery } from '@wasp/queries'

import Navbar from './Navbar'
import ArticleListPaginated from './ArticleListPaginated'
import smileyImageUrl from './smiley.jpg'

const UserProfilePage = (props) => {
  const history = useHistory()

  const { data: me } = useAuth()

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

  return user ? (
    <div>
      <Navbar />

      <img src={user.profilePictureUrl || smileyImageUrl} />
      <p> { user.username } </p>
      <p> { user.bio } </p>
      { me && me.username === username && (
        <div>
          <Link to='/settings'>Edit Profile Settings</Link>
        </div>
      )}
      { me && me.username !== username && (
        <div>
          <FollowUserButton user={user} />
        </div>
      )}

      <Articles user={user} />
    </div>
  ) : null
}

const FollowUserButton = (props) => {
  const user = props.user
  const { data: me } = useAuth()

  const toggleFollow = async () => {
    try {
      followUser({ username: user.username, follow: !user.following })
    } catch (err) {
      console.error(err)
      window.alert(err)
    }
  }

  return me && me.username !== user.username ? (
    <button onClick={toggleFollow}>
      { user.following ? 'Unfollow' : 'Follow' }
    </button>
  ) : null
}

const Articles = (props) => {
  const user = props.user

  return (
    <div>
      <h1> My Articles </h1>
      <ArticleListPaginated
        query={getArticlesByUser}
        makeQueryArgs={({ skip, take }) => ({ username: props.user.username, skip, take })}
        pageSize={2}
      />
      <h1> Favorited Articles </h1>
      <ArticleListPaginated
        query={getFavoritedArticles}
        makeQueryArgs={({ skip, take }) => ({ username: props.user.username, skip, take })}
        pageSize={2}
      />
    </div>
  )
}

export default UserProfilePage
