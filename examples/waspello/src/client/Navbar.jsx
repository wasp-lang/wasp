import React from 'react'

import logout from '@wasp/auth/logout.js'

import logo from './waspello-logo-navbar.svg'
import './Navbar.css'

const Navbar = ({ user }) => {

  return (
    <div className="navbar">
      <div className='navbar-item'>
        <span>Home</span>
      </div>

      <img alt="Waspello" className="navbar-logo navbar-item" src={logo} />

      <div className='navbar-item'>
        <span>
          { user.username }
          &nbsp;|&nbsp;
          <button className="logout-btn" onClick={logout}> logout </button>
        </span>
      </div>
    </div>
  )
}

export default Navbar
