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
  //   When is article null, when not, should I look into combination of article and articleSlug, then
  //   there is this 'enabled' which I need on the other hand -> uff. And what if I get error? humpf!
  const articleSlug = props.match.params.articleSlug
  const { data: article, error: articleError } = useQuery(getArticle, { slug: articleSlug }, { enabled: articleSlug })

  // TODO: Instead of this logic here, I wish I could use ACL via Wasp and just
  //   receive user via props instead of useAuth().
  if (!user || isError) {
    return <span> Please <Link to='/login'>log in</Link>. </span>
  }

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

  const history = useHistory()

  const [title, setTitle] = useState(article?.title || '')
  const [description, setDescription] = useState(article?.description || '')
  const [markdownContent, setMarkdownContent] = useState(article?.markdownContent || '')
  const [tags, setTags] = useState(article?.tags || [])
  const [newTagName, setNewTagName] = useState('')

  const [submitError, setSubmitError] = useState(null)

  const handleSubmit = async (event) => {
    event.preventDefault()
    setSubmitError(null)
    try {
      let articleSlug
      if (article?.id) {
        await updateArticle({
          id: article.id,
          title,
          description,
          markdownContent,
          tags
        })
        articleSlug = article.slug
      } else {
        const newArticle = await createArticle({
          title,
          description,
          markdownContent,
          tags
        })
        articleSlug = newArticle.slug
      }
      history.push(`/article/${articleSlug}`)
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
        <textarea
          value={markdownContent}
          onChange={e => setMarkdownContent(e.target.value)}
        />

        <h2>Enter tags</h2>
        <input
          type="text"
          value={newTagName}
          onChange={e => setNewTagName(e.target.value)}
          onKeyPress={e => {
            if (e.key === 'Enter') {
              e.preventDefault()
              setTags([...tags, { name: newTagName }])
              setNewTagName('')
            }
          }}
        />
        <div>
          { tags.map(tag => (
            <div key={tag.name}>
              {tag.name}
              <button onClick={() => setTags(tags.filter(t => t !== tag))}> X </button>
            </div>
          ))}
        </div>

        <div>
          <input type='submit' value='Publish Article' />
        </div>
      </form>
    </div>
  )
}

export default ArticleEditorPage
