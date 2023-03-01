import React from 'react'
import ColorHash from 'color-hash'
import { Link } from 'react-router-dom'

import './Tag.css'

const getClasses = (href, isActive, isDeletable) => [
  'tag',
  href ? 'link' : '',
  isActive ? 'active': '',
  isDeletable ? 'deletable' : '' 
].join(' ')
const getTagColor = (name) => new ColorHash().hex(name)

const Tag = ({
  name,
  href,
  onClick,
  isActive,
  isDeletable
}) => {
  return (
    <Link
      to={href ? href : '#'}
      className={getClasses(href, isActive, isDeletable)}
      style={{ color: getTagColor(name) }}
      onClick={onClick}
    >
      {name === 'all' ? `${name}` : `#${name}`}
    </Link>
  )
}

export default Tag
