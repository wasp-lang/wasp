import React from 'react'
import { Link } from 'react-router-dom'
import moment from 'moment'

import setArticleFavorited from '@wasp/actions/setArticleFavorited'

import smileyImageUrl from './smiley.jpg'

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
    <div style={{ border: '1px solid black' }}>
      <Link to={`/article/${article.slug}`}>
        <h2> { article.title } </h2>
      </Link>
      <p> { article.description } </p>
      <p>
        <em> Tags: </em>
        { article.tags.map(t => t.name).join('; ') }
      </p>
      <p>
        <img src={ article.user.profilePictureUrl || smileyImageUrl } width='30px' />
        <div> { article.user.username } </div>
        <div> { moment(article.createdAt).format('MMMM DD, YYYY') } </div>
      </p>
      <div>
        <button onClick={toggleArticleFavorited}>
          { article.favorited ? 'Unlike' : 'Like' } ({ article.favoritesCount })
        </button>
      </div>
    </div>
  )
}

export default ArticleList
