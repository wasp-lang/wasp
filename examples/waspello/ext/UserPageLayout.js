import React from 'react'

import Navbar from './Navbar'
import WaspExampleHeader from './WaspExampleHeader.js'

import './UserPageLayout.css'

const UserPageLayout = ({ user, children }) => (
  <div className='layout-root'>
    <WaspExampleHeader name="Waspello" />

    <Navbar user={user} />
    <div className='layout-content'>
      {children}
    </div>
  </div>
)

export default UserPageLayout
