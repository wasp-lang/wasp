import React from 'react'
import { Link } from 'react-router-dom'

import './TagsSidebar.css'

import getTags from '@wasp/queries/getTags'
import { useQuery } from '@wasp/queries'
import Tag from './Tag'

// Props: active  TODO: document this properly.
const TagsSidebar = (props) => {
  const { data: tags } = useQuery(getTags)

  return (
    <div className="tags-sidebar">
      <Link to="/" className="new-thought-link"> New thought </Link>
      <Tag
        name={'all'}
        href={'/thoughts'}
        isActive={props.active === '_all'}
        key={'_all'}
      />
      { tags && tags.map(tag => (
        <Tag
          name={tag.name}
          href={`/thoughts?tag=${tag.name}`}
          isActive={props.active === tag.name}
          key={tag.name}
        />
      ))}
    </div>
  )
}

export default TagsSidebar
