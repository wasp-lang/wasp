import React from 'react'
import { Link } from 'react-router-dom'

import './TagsSidebar.css'
import { getTagColor } from './tag.js'

import getTags from '@wasp/queries/getTags'
import { useQuery } from '@wasp/queries'

// Props: active  TODO: document this properly.
const TagsSidebar = (props) => {
  const { data: tags } = useQuery(getTags)

  return (
    <div className="tags-sidebar">
      <Link to="/" className="new-thought-link"> New thought </Link>
      <Link to={`/thoughts`}
            className={`tags-sidebar-tag tags-sidebar-tag-all ${props.active === '_all' ? 'active' : ''}`}
            style={{ color: 'black' }}
            key="_all">
        all
      </Link>
      { tags && tags.map(tag => (
        <Link to={`/thoughts?tag=${tag.name}`}
              className={`tags-sidebar-tag ${props.active === tag.name ? 'active' : ''}`}
              style={{ color: getTagColor(tag.name)}}
              key={tag.name}>
          #{tag.name}
        </Link>
      ))}
    </div>
  )
}

export default TagsSidebar
