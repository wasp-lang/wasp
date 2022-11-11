import React from 'react'

import logout from '@wasp/auth/logout.js'
import Todo from '../Todo.js'
import '../Main.css'

const Main = () => {
  return (
    <>
      <button className='btn btn-blue' onClick={logout}>
        Logout
      </button>
      <Todo />
    </>
  )
}

export default Main
