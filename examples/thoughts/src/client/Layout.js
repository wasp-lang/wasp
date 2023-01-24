import React from 'react'
import TopNavbar from './TopNavbar'
import TagsSidebar from './TagsSidebar'
import addWaspSourceHeader from './addWaspSourceHeader.js'
import './Layout.css'

const Layout = ({ user, activeTag, children }) => (
  <div className='layout-root'>
    <TopNavbar user={user} />
    <div className='layout-content'>
      <TagsSidebar active={activeTag} />
      {children}
    </div>
  </div>
)

export default addWaspSourceHeader(Layout)
