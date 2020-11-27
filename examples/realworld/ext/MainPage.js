import React, { useState } from 'react'
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

  const allArticlesPageSize = 1
  // TODO: Make the page idx persistent in the URL as query param.
  const [allArticlesPageIdx, setAllArticlesPageIdx] = useState(0)
  const { data: allArticlesData } = useQuery(
    getAllArticles,
    {
      skip: allArticlesPageIdx * allArticlesPageSize,
      take: allArticlesPageSize
    }
  )
  const allArticles = allArticlesData?.articles
  const allArticlesCount = allArticlesData?.count
  const allArticlesPageCount = Math.trunc(allArticlesCount / allArticlesPageSize)
  console.log(allArticles, allArticlesCount, allArticlesPageCount)

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
        { allArticlesPageCount > 0 && (
          <div>
            { allArticlesPageIdx > 0 && (
              <>
                <button> 1 </button>
                <button> &lt; </button>
              </>
            ) }
            { /* TODO: Make the current page number an input which user can change. */ }
            { allArticlesPageIdx + 1 }
            { allArticlesPageIdx < allArticlesPageCount - 1 && (
              <>
                <button> &gt; </button>
                <button> { allArticlesPageCount } </button>
              </>
            )}
          </div>
        ) }
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
