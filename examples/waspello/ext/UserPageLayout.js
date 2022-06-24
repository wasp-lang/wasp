import React from 'react'

import Navbar from './Navbar'
import WaspSourceHeader from './WaspSourceHeader.js'

import './UserPageLayout.css'

const UserPageLayout = ({ user, children }) => (
  <div className='layout-root'>
    <WaspSourceHeader name="Waspello" />

    <Navbar user={user} />
    <div className='layout-content'>
      {children}
    </div>
  </div>
)

export default UserPageLayout
