import React from 'react'
import { Link } from 'react-router-dom'
import _ from 'lodash'

import useAuth from '@wasp/auth/useAuth.js'
import getTags from '@wasp/queries/getTags'
import { useQuery } from '@wasp/queries'

import Navbar from './Navbar'

const MainPage = () => {
  const { data: user } = useAuth()


  return (
    <div>
      <Navbar />

      <Tags />

      TODO: Main page
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
