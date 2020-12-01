import React, { useState } from 'react'
import { Link } from 'react-router-dom'
import _ from 'lodash'

import useAuth from '@wasp/auth/useAuth.js'
import { useQuery } from '@wasp/queries'

import getTags from '@wasp/queries/getTags'
import getFollowedArticles from '@wasp/queries/getFollowedArticles'
import getAllArticles from '@wasp/queries/getAllArticles'
import Navbar from './Navbar'
import ArticleListPaginated from './article/components/ArticleListPaginated'

const MainPage = () => {
  const { data: me } = useAuth()

  return (
    <div>
      <Navbar />

      <Tags />

      { me && (
        <div>
          <h1> Your Feed </h1>
          <ArticleListPaginated
            query={getFollowedArticles}
            makeQueryArgs={({ skip, take }) => ({ skip, take })}
            pageSize={2}
          />
        </div>
      )}

      <div>
        <h1> Global Feed </h1>
        <ArticleListPaginated
          query={getAllArticles}
          makeQueryArgs={({ skip, take }) => ({ skip, take })}
          pageSize={2}
        />
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
