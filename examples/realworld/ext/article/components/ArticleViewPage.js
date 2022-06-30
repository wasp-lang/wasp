import React, { useState } from 'react'
import { useHistory } from 'react-router-dom'
import ReactMarkdown from 'react-markdown'
import moment from 'moment'
import PropTypes from 'prop-types'

import Container from '@material-ui/core/Container'
import Grid from '@material-ui/core/Grid'
import Card from '@material-ui/core/Card'
import CardContent from '@material-ui/core/CardContent'
import CardActions from '@material-ui/core/CardActions'
import CardHeader from '@material-ui/core/CardHeader'
import TextField from '@material-ui/core/TextField'
import Typography from '@material-ui/core/Typography'
import Avatar from '@material-ui/core/Avatar'
import Chip from '@material-ui/core/Chip'
import Button from '@material-ui/core/Button'
import { makeStyles } from '@material-ui/core/styles'

import useAuth from '@wasp/auth/useAuth.js'
import { useQuery } from '@wasp/queries'

import getArticle from '@wasp/queries/getArticle'
import getArticleComments from '@wasp/queries/getArticleComments'
import deleteArticle from '@wasp/actions/deleteArticle'
import createComment from '@wasp/actions/createComment'
import deleteComment from '@wasp/actions/deleteComment'

import Navbar from '../../Navbar'
import addWaspSourceHeader from '../../addWaspSourceHeader.js'

const useStyles = makeStyles((theme) => ({
  tags: {
    '& *:not(:last-child)': {
      marginRight: theme.spacing(0.5)
    },
    marginBottom: theme.spacing(3)
  },
  comments: {
    '& *:not(:last-child)': {
      marginBottom: theme.spacing(0.5)
    },
  },
  ownArticleButtons: {
    '& *:not(:last-child)': {
      marginRight: theme.spacing(0.5)
    },
    marginBottom: theme.spacing(3)
  },
  textField: {
    marginBottom: theme.spacing(3)
  },
  postCommentButton: {
    marginBottom: theme.spacing(3)
  }
}))

const ArticleViewPage = (props) => {
  const classes = useStyles()

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
      history.push('/')
    } catch (err) {
      console.log(err)
      window.alert('Failed to delete article: ' + err)
    }
  }

  return article ? (
    <Container maxWidth="lg">
      <Navbar />

      <Grid container direction="row" justify="center">
        <Grid item xs={8}>
          <Typography variant="h2">{ article.title }</Typography>

          <div>
            <div> Author: { article.user.username } </div>
            <div> Created at: { moment(article.createdAt).format('MMMM DD, YYYY') } </div>
          </div>
        </Grid>

        <Grid item xs={8}>
          <ReactMarkdown children={article.markdownContent} />
        </Grid>

        <Grid item xs={8}>
          <div className={classes.tags}>
            <span>Tags:</span>
            { article.tags.map(tag => <Chip label={tag.name} />) }
          </div>

          { isMyArticle && (
            <div className={classes.ownArticleButtons}>
              <Button color="primary" variant="outlined" onClick={handleEditArticle}>
                Edit Article
              </Button>
              <Button color="secondary" variant="outlined" onClick={handleDeleteArticle}>
                Delete Article
              </Button>
            </div>
          )}

          <Comments article={article}/>
        </Grid>

      </Grid>
    </Container>
  ) : null
}

const Comments = (props) => {
  const classes = useStyles()

  const article = props.article

  const { data: me } = useAuth()

  const { data: comments } = useQuery(getArticleComments, { articleId: article.id })

  return (
    <div>
      { me
        ? <CreateComment article={article} />
        : null // TODO: Instead of nothing, tell them they need to sign up / sign in to comment.
      }
      { comments ? (
        <div className={classes.comments}>
          { comments.length
            ? comments.map(c => <Comment comment={c} key={c.id} />)
            : 'No comments yet!'
          }
        </div>
      ) : null }
    </div>
  )
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
    <>
      <Card>
        <CardHeader
          avatar={<Avatar>R</Avatar>}
          title={comment.user.username}
          subheader={ moment(comment.createdAt).format('MMMM DD, YYYY') }
        />

        <CardContent>
          <Typography variant="body1">
            { comment.content }
          </Typography>
        </CardContent>

        <CardActions>
          { (me && me.id === comment.userId)
            ? <Button size="small" color="primary" onClick={onDelete}>Delete</Button>
            : null
          }
        </CardActions>
      </Card>
    </>
  )
}
Comment.propTypes = {
  comment: PropTypes.object.isRequired
}

const CreateComment = (props) => {
  const classes = useStyles()

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
      <TextField
        className={classes.textField}
        label='Leave a comment'
        multiline
        fullWidth
        rows={3}
        value={content}
        onChange={e => setContent(e.target.value)}
      />
      <div className={classes.postCommentButton}>
        <Button type="submit" color="primary" variant="contained">Post Comment</Button>
      </div>
    </form>
  )
}
CreateComment.propTypes = {
  article: PropTypes.object.isRequired
}

export default addWaspSourceHeader(ArticleViewPage)
