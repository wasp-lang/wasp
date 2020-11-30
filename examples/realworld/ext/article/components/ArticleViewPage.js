import React, { useState } from 'react'
import { useHistory } from 'react-router-dom'
import ReactMarkdown from 'react-markdown'
import moment from 'moment'
import PropTypes from 'prop-types'

import useAuth from '@wasp/auth/useAuth.js'
import { useQuery } from '@wasp/queries'

import getArticle from '@wasp/queries/getArticle'
import getArticleComments from '@wasp/queries/getArticleComments'
import getUser from '@wasp/queries/getUser'
import deleteArticle from '@wasp/actions/deleteArticle'
import createComment from '@wasp/actions/createComment'
import deleteComment from '@wasp/actions/deleteComment'

import Navbar from '../../Navbar'

const ArticleViewPage = (props) => {
  const history = useHistory()
  const { data: me } = useAuth({ keepPreviousData: true })

  const articleSlug = props.match.params.articleSlug
  const { data: article } = useQuery(getArticle, { slug: articleSlug })

  // TODO: If there is no such article, we get null here under `article`,
  //   and we don't handle that properly, we just return blank screen (return null).
  //   How should we detect this and handle it?
  //   Should we modify action to return error instead of null?
  //   Or should we check for (!isLoading && !article)?
  //   What do we even do in such situation?
  //   Or maybe we should make it so that every operations returns an object always,
  //   and that object contains article then -> then it is very clear if something got returned,
  //   or if it is that initial null.

  const isMyArticle = me?.id && (me?.id === article?.userId)

  const handleEditArticle = () => {
    history.push(`/editor/${article.slug}`)
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
        <div> Author: { article.user.username } </div>
        <div> Created at: { moment(article.createdAt).format('MMMM DD, YYYY') } </div>
      </div>

      <div>
        <p> { article.title } </p>
        <p> { article.description } </p>
        <p>
          <ReactMarkdown children={article.markdownContent} />
        </p>
        <p>
          Tags: { article.tags.map(tag => <div> {tag.name} </div>) }
        </p>
      </div>

      { isMyArticle && (
        <div>
          <button onClick={handleEditArticle}> Edit Article </button>
          <button onClick={handleDeleteArticle}> Delete Article </button>
        </div>
      )}

      <Comments article={article}/>
    </div>
  ) : null
}

const Comments = (props) => {
  const article = props.article

  const { data: me } = useAuth()

  const { data: comments } = useQuery(getArticleComments, { articleId: article.id })

  return comments ? (
    <div>
      { me
        ? <CreateComment article={article} />
        : null // TODO: Instead of nothing, tell them they need to sign up / sign in to comment.
      }

      <div>
        { comments.length
          ? comments.map(c => <Comment comment={c} key={c.id} />)
          : 'No comments yet!'
        }
      </div>
    </div>
  ) : null
}
Comments.propTypes = {
  article: PropTypes.object.isRequired
}

const Comment = (props) => {
  const comment = props.comment

  const { data: me } = useAuth()

  const onDelete = async () => {
    try {
      await deleteComment({ id: comment.id })
    } catch (err) {
      console.error(err)
      window.alert(err)
    }
  }

  return (
    <div style={{ border: '1px solid black', width: '300px' }}>
      <div> { comment.content } </div>
      <div> { moment(comment.createdAt).format('MMMM DD, YYYY') } </div>
      { /* TODO: Show user's profile picture. */ }
      { /* TODO: Make username a link to the user profile. */ }
      <div> { comment.user.username } </div>
      { (me && me.id === comment.userId)
        ? <button onClick={onDelete}> Delete </button>
        : null
      }
    </div>
  )
}
Comment.propTypes = {
  comment: PropTypes.object.isRequired
}

const CreateComment = (props) => {
  const article = props.article

  const [content, setContent] = useState('')

  const handleSubmit = async (event) => {
    event.preventDefault()
    try {
      await createComment({ articleId: article.id, content })
    } catch (err) {
      console.error(err)
      window.alert(err)
    }
  }

  return (
    <form onSubmit={handleSubmit}>
      <textarea
        value={content}
        onChange={e => setContent(e.target.value)}
        style={{ width: '300px' }}
      />
      <div>
        <input type='submit' value='Post Comment' />
      </div>
    </form>
  )
}
CreateComment.propTypes = {
  article: PropTypes.object.isRequired
}

export default ArticleViewPage
