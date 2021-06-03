import React from 'react'
import TopNavbar from './TopNavbar'
import TagsSidebar from './TagsSidebar'
import './Layout.css'

const Layout = ({ user, activeTag, children }) => (
  <div className='base-page'>
    <TopNavbar user={user} />
    <div className='base-container'>
      <TagsSidebar active={activeTag} />
      {children}
    </div>
  </div>
)

export default Layout
