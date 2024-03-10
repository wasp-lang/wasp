import React from 'react'

import Navbar from '../common/Navbar'
import addWaspSourceHeader from '../common/addWaspSourceHeader'

import './UserPageLayout.css'

const UserPageLayout = ({ user, children }) => (
  <div className='layout-root'>

    <Navbar user={user} />
    <div className='layout-content'>
      {children}
    </div>
  </div>
)

export default addWaspSourceHeader(UserPageLayout)
