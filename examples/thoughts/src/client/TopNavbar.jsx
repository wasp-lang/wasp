import React from 'react'

import logout from '@wasp/auth/logout'

import './TopNavbar.css'


const TopNavbar = (props) => {
  const user = props.user

  return (
    <div className="top-navbar">
      { user.username }
      &nbsp;|&nbsp;
      <button className="plain" onClick={logout}> logout </button>
    </div>
  )
}

export default TopNavbar
