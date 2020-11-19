import React from 'react'
import { Link } from 'react-router-dom'

import useAuth from '@wasp/auth/useAuth.js'

import Navbar from './Navbar'

const MainPage = () => {
  const { data: user } = useAuth()

  return (
    <div>
      <Navbar />

      TODO: Main page
    </div>
  )
}

export default MainPage
