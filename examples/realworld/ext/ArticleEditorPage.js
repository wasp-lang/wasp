import React, { useState } from 'react'
import { Link, useHistory } from 'react-router-dom'

import useAuth from '@wasp/auth/useAuth.js'
import logout from '@wasp/auth/logout.js'
import createArticle from '@wasp/actions/createArticle'
import updateArticle from '@wasp/actions/updateArticle'
import { useQuery } from '@wasp/queries'
import getArticle from '@wasp/queries/getArticle'

import Navbar from './Navbar'

const ArticleEditorPage = (props) => {
  const { data: user, isError } = useAuth({ keepPreviousData: true })

  // TODO: Here, as in some other places, it feels tricky to figure out what is happening regarding the state.
  //   When is article null, when not, should I look into combination of article and articleId, then
  //   there is this 'enabled' which I need on the other hand -> uff. And what if I get error? humpf!
  const articleId = parseInt(props.match.params.articleId)
  const { data: article, error: articleError } = useQuery(getArticle, { id: articleId }, { enabled: articleId })

  // TODO: Instead of this logic here, I wish I could use ACL via Wasp and just
  //   receive user via props instead of useAuth().
  if (!user || isError) {
    return <span> Please <Link to='/login'>log in</Link>. </span>
  }

  console.log('here', article)

  return articleError
    ? articleError.message || articleError
    : (
      <div>
        <Navbar />
        <ArticleEditor user={user} article={article} />
      </div>
    )
}

const ArticleEditor = (props) => {
  const user = props.user
  const article = props.article

  console.log(article)

  const history = useHistory()

  const [title, setTitle] = useState(article?.title || '')
  const [description, setDescription] = useState(article?.description || '')
  const [markdownContent, setMarkdownContent] = useState(article?.markdownContent || '')

  const [submitError, setSubmitError] = useState(null)

  const handleSubmit = async (event) => {
    event.preventDefault()
    setSubmitError(null)
    try {
      let articleId
      if (article?.id) {
        await updateArticle({
          id: article.id,
          title,
          description,
          markdownContent
        })
        articleId = article.id
      } else {
        const newArticle = await createArticle({
          title,
          description,
          markdownContent
        })
        articleId = newArticle.id
      }
      history.push(`/article/${articleId}`)
    } catch (err) {
      setSubmitError(err)
    }
  }

  return (
    <div>
      { submitError && (
        <p>
          { submitError.message || submitError }
        </p>
      ) }

      <form onSubmit={handleSubmit}>
        <h2>Article title</h2>
        <input
          type='text'
          value={title}
          onChange={e => setTitle(e.target.value)}
        />

        <h2>What's this article about?</h2>
        <input
          type='text'
          value={description}
          onChange={e => setDescription(e.target.value)}
        />

        <h2>Markdown content</h2>
        <input
          type='text'
          value={markdownContent}
          onChange={e => setMarkdownContent(e.target.value)}
        />

        <div>
          <input type='submit' value='Publish Article' />
        </div>
      </form>
    </div>
  )
}

export default ArticleEditorPage
