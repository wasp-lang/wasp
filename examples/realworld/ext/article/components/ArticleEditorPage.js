import React, { useState } from 'react'
import PropTypes from 'prop-types'
import { useHistory } from 'react-router-dom'

import Container from '@material-ui/core/Container'
import TextField from '@material-ui/core/TextField'
import Grid from '@material-ui/core/Grid'
import Chip from '@material-ui/core/Chip'
import Button from '@material-ui/core/Button'
import { makeStyles } from '@material-ui/core/styles'

import { useQuery } from '@wasp/queries'

import createArticle from '@wasp/actions/createArticle'
import updateArticle from '@wasp/actions/updateArticle'
import getArticle from '@wasp/queries/getArticle'

import Navbar from '../../Navbar'
import addWaspSourceHeader from '../../addWaspSourceHeader'

const useStyles = makeStyles((theme) => ({
  /*
  root: {
    display: 'flex',
    flexWrap: 'wrap',
  },
  */
  textField: {
    //marginLeft: theme.spacing(1),
    //width: '25ch',
    marginBottom: theme.spacing(3)
  },

  tags: {
    '& *:not(:last-child)': {
      marginRight: theme.spacing(0.5)
    },
    marginBottom: theme.spacing(3)
  }
}))

const ArticleEditorPage = (props) => {
  // TODO: Here, as in some other places, it feels tricky to figure out what is happening regarding the state.
  //   When is article null, when not, should I look into combination of article and articleSlug, then
  //   there is this 'enabled' which I need on the other hand -> uff. And what if I get error? humpf!
  const articleSlug = props.match.params.articleSlug
  const { data: article, error: articleError } = useQuery(getArticle, { slug: articleSlug }, { enabled: !!articleSlug })

  return articleError
    ? articleError.message || articleError
    : (
      <Container maxWidth="lg">
        <Navbar />
        <Grid container direction="row" justify="center">
          <Grid item xs={8}>
            <ArticleEditor user={props.user} article={article} />
          </Grid>
        </Grid>
      </Container>
    )
}

ArticleEditorPage.propTypes = {
  user: PropTypes.object
}

const ArticleEditor = (props) => {
  const classes = useStyles()

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
        <TextField
          className={classes.textField}
          label="Article Title"
          fullWidth
          value={title} 
          onChange={e => setTitle(e.target.value)}
        />

        <TextField
          className={classes.textField}
          label="What's this article about"
          fullWidth
          value={description} 
          onChange={e => setDescription(e.target.value)}
        />

        <TextField
          className={classes.textField}
          label="Markdown content"
          multiline
          rows={3}
          fullWidth
          value={markdownContent} 
          onChange={e => setMarkdownContent(e.target.value)}
        />

        <TextField
          className={classes.textField}
          label="Enter tags"
          fullWidth
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

        <div className={classes.tags}>
          { tags.map(tag => (
            <Chip label={tag.name} onDelete={() => setTags(tags.filter(t => t !== tag))}/>
          ))}
        </div>

        <Button type='submit' color='primary' variant='contained'>Publish Article</Button>

      </form>
    </div>
  )
}

export default addWaspSourceHeader(ArticleEditorPage)
