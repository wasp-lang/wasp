import React from 'react'
import { Link } from 'react-router-dom'
import _ from 'lodash'

import useAuth from '@wasp/auth/useAuth.js'
import getTags from '@wasp/queries/getTags'
import getFollowedArticles from '@wasp/queries/getFollowedArticles'
import getAllArticles from '@wasp/queries/getAllArticles'
import { useQuery } from '@wasp/queries'

import Navbar from './Navbar'
import ArticleList from './ArticleList'

const MainPage = () => {
  const { data: me } = useAuth()

  const { data: followedArticles } = useQuery(getFollowedArticles)
  const { data: allArticles } = useQuery(getAllArticles)

  return (
    <div>
      <Navbar />

      <Tags />

      { me && (
        <div>
          <h1> Your Feed </h1>
          <ArticleList articles={followedArticles || []} />
        </div>
      )}

      <div>
        <h1> Global Feed </h1>
        <ArticleList articles={allArticles || []} />
      </div>

    </div>
  )
}

const Tags = () => {
  const { data: tags } = useQuery(getTags)

  if (!tags) return null

  const popularTags = _.take(_.sortBy(tags, [t => -1 * t.numArticles]), 10)

  return (
    <div>
      Popular tags: { popularTags.map(tag => (
        <div>
          { tag.name } ({ tag.numArticles })
        </div>
      ))}
    </div>
  )
}

export default MainPage
