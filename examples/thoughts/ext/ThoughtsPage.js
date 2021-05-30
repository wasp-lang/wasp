import React from 'react'
import ReactMarkdown from 'react-markdown'
import { useLocation } from 'react-router-dom'

import './Main.css'
import './ThoughtsPage.css'
import { getTagColor } from './tag.js'
import TagsSidebar from './TagsSidebar.js'
import TopNavbar from './TopNavbar.js'

import getThoughts from '@wasp/queries/getThoughts'
import { useQuery } from '@wasp/queries'

const ThoughtsPage = (props) => {
  const queryParams = new URLSearchParams(useLocation().search)
  const tag = queryParams.get('tag')

  // TODO: Handle possible errors and fetching.
  const { data: thoughts } = useQuery(getThoughts, { tagName: tag })

  // TODO: Duplication! layout, navbar, sidebar, ...
  return (
    <div className="main-page">
      <TopNavbar user={props.user} />

      <div className="main-container">
        <TagsSidebar active={tag || '_all'} />

        <div className="center-container">
          <ThoughtsList thoughts={thoughts} />
        </div>
      </div>
    </div>
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
