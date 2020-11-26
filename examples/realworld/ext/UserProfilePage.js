import React, { useState } from 'react'
import { Link, useHistory } from 'react-router-dom'

import useAuth from '@wasp/auth/useAuth.js'
import logout from '@wasp/auth/logout.js'
import updateUser from '@wasp/actions/updateUser'
import getUser from '@wasp/queries/getUser'
import getArticlesByUser from '@wasp/queries/getArticlesByUser'
import getFavoritedArticles from '@wasp/queries/getFavoritedArticles'
import setArticleFavorited from '@wasp/actions/setArticleFavorited'
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
        { /* TODO: Show this link only if user is logged in. */ }
        <Link to='/settings'>Edit Profile Settings</Link>
      </div>

      <Articles user={user} />
    </div>
  ) : null
}

const Articles = (props) => {
  const user = props.user

  const { data: myArticles } = useQuery(getArticlesByUser, { username: props.user.username })
  const { data: favoritedArticles } = useQuery(getFavoritedArticles, { username: props.user.username })

  return (
    <div>
      <h1> My Articles </h1>
      <ArticleList articles={myArticles} />
      <h1> Favorited Articles </h1>
      <ArticleList articles={favoritedArticles} />
    </div>
  )
}

const ArticleList = (props) => {
  const articles = props.articles
  // TODO: Should I have pagination here, probably I should?
  return articles ? (
    <div>
      { articles.map(article => <Article article={article} key={article.id} />) }
    </div>
  ) : null
}

const Article = (props) => {
  const article = props.article

  const toggleArticleFavorited = async () => {
    await setArticleFavorited({ id: article.id, favorited: !article.favorited })
  }

  return (
    <div>
      <Link to={`/article/${article.slug}`}>
        <h2> { article.title } </h2>
      </Link>
      <p> { article.description } </p>
      <div>
        <button onClick={toggleArticleFavorited}>
          { article.favorited ? 'Unlike' : 'Like' } ({ article.favoritesCount })
        </button>
      </div>
    </div>
  )
}

export default UserProfilePage
