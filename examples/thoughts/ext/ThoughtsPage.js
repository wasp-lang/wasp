import React from 'react'
import Layout from './Layout'
import ReactMarkdown from 'react-markdown'
import { useLocation } from 'react-router-dom'

import './ThoughtsPage.css'
import { getTagColor } from './tag.js'

import getThoughts from '@wasp/queries/getThoughts'
import { useQuery } from '@wasp/queries'

const ThoughtsPage = (props) => {
  const queryParams = new URLSearchParams(useLocation().search)
  const tag = queryParams.get('tag')

  // TODO: Handle possible errors and fetching.
  const { data: thoughts } = useQuery(getThoughts, { tagName: tag })

  return (
    <Layout
      user={props.user}
      activeTag={tag || '_all'}
    >
      <div className="center-container">
        <ThoughtsList thoughts={thoughts} />
      </div>
    </Layout>
  )
}

const ThoughtsList = ({ thoughts }) => {
  return (
    <div className="thoughts-list">
      { thoughts?.length ? thoughts.map((thought, idx) =>
        <ThoughtListView thought={thought} key={thought.id} />
      ) : 'No thoughts to show'
      }
    </div>
  )
}

const ThoughtListView = (props) => (
  <div className="thought-list-view">
    <div className="thought-list-view-tags">
      {props.thought.tags?.map(tag => (
        <div className="thought-list-view-tags-tag"
             style={{ color: getTagColor(tag.name)}}
             key={tag.name}>
          {tag.name}
        </div>
      ))}
    </div>

    <div className="thought-list-view-text">
      <ReactMarkdown children={props.thought.textMarkdown} />
    </div>
  </div>
)

export default ThoughtsPage
