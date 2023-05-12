import { Link } from 'react-router-dom'
import { useEffect } from 'react'

import logout from '@wasp/auth/logout.js'
import useAuth from '@wasp/auth/useAuth'
import { useQuery } from '@wasp/queries'
import getDate from '@wasp/queries/getDate'
import useSocket from './useSocket'

import './Main.css'

export function App({ children }: any) {
  const { data: user } = useAuth()
  const { data: date } = useQuery(getDate)
  const [isConnected, socket] = useSocket()

  useEffect(() => {
    socket.emit('ping', 'hello from App.tsx')
    socket.on('pong', (data: any) => {
      console.log('pong', data)
    })
    return () => {
      socket.off('pong')
    }
  }, [])

  return (
    <div className="app border-spacing-2 p-4">
      <p>Connected: {`${isConnected}`}</p>
      <header className="flex justify-between">
        <h1 className="font-bold text-3xl mb-5">
          <Link to="/">ToDo App</Link>
        </h1>
        <h2>
          Your site was loaded at: {date?.toLocaleString()}
        </h2>
        {user && (
          <div className="flex gap-3 items-center">
            <div>
              Hello, <Link to="/profile">{user.email}</Link>
            </div>
            <div>
              <button className="btn btn-primary" onClick={logout}>
                Logout
              </button>
            </div>
          </div>
        )}
      </header>
      <main>{children}</main>
      <footer className="mt-8 text-center">
        Created with Wasp
      </footer>
    </div>
  )
}
