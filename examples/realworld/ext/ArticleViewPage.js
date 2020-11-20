import React from 'react'
import { useHistory } from 'react-router-dom'

import useAuth from '@wasp/auth/useAuth.js'
import { useQuery } from '@wasp/queries'

import getArticle from '@wasp/queries/getArticle'
import deleteArticle from '@wasp/actions/deleteArticle'

import Navbar from './Navbar'

const ArticleViewPage = (props) => {
  const history = useHistory()
  const { data: user } = useAuth({ keepPreviousData: true })

  const articleId = parseInt(props.match.params.articleId)
  const { data: article } = useQuery(getArticle, { id: articleId })

  // TODO: If there is no such article, we get null here under `article`,
  //   and we don't handle that properly, we just return blank screen (return null).
  //   How should we detect this and handle it?
  //   Should we modify action to return error instead of null?
  //   Or should we check for (!isLoading && !article)?
  //   What do we even do in such situation?
  //   Or maybe we should make it so that every operations returns an object always,
  //   and that object contains article then -> then it is very clear if something got returned,
  //   or if it is that initial null.

  const isMyArticle = user?.id && (user?.id === article?.userId)

  const handleEditArticle = () => {
    history.push(`/editor/${article.id}`)
  }

  const handleDeleteArticle = async () => {
    if (!window.confirm('Are you sure you want to delete the article?')) return
    try {
      await deleteArticle({ id: article.id })
    } catch (err) {
      console.log(err)
      window.alert('Failed to delete article: ' + err)
    }
  }

  return article ? (
    <div>
      <Navbar />

      <div>
        <p> { article.title } </p>
        <p> { article.description } </p>
        <p> { article.markdownContent } </p>
      </div>

      { isMyArticle && (
        <div>
          <button onClick={handleEditArticle}> Edit Article </button>
          <button onClick={handleDeleteArticle}> Delete Article </button>
        </div>
      )}
    </div>
  ) : null
}

export default ArticleViewPage
