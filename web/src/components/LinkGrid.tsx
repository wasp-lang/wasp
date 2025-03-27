import React from 'react'
import './LinkGrid.css'
import Link from '@docusaurus/Link';

export interface LinkInfo {
  title: string
  description?: string
  link: string
}

export function LinkGrid({
  links,
  caption = 'Click on each box for more details.',
}: {
  links: LinkInfo[]
  caption?: string | false
}) {
  return (
    <>
      <div className="link-grid-layout">
        {links.map((link) => (
          <LinkGridBox link={link} />
        ))}
      </div>
      {caption ? (
        <p className="link-grid-info">
          <small>{caption}</small>
        </p>
      ) : null}
    </>
  )
}

function LinkGridBox({
  link: { title, description, link },
}: {
  link: LinkInfo
}) {
  return (
    <Link href={link} className="link-grid-box">
      <h3>{title} »</h3>
      {description ? <p>{description}</p> : null}
    </Link>
  )
}
