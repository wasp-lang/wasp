import { Outlet } from 'react-router-dom'
import { env } from 'wasp/client'
import { logout, useAuth } from 'wasp/client/auth'
import { getDate, useQuery } from 'wasp/client/operations'
import { Link } from 'wasp/client/router'
import { useSocket } from 'wasp/client/webSocket'

import './Main.css'
import { getName } from './user'
// Necessary to trigger type tests.
import './testTypes/operations/client'

export function App() {
  const { data: user } = useAuth()
  const { data: date } = useQuery(getDate)
  const { isConnected } = useSocket()

  const connectionIcon = isConnected ? '🟢' : '🔴'

  const appName = env.REACT_APP_NAME

  return (
    <div className="app border-spacing-2 p-4">
      <header className="flex justify-between">
        <h1 className="font-bold text-3xl mb-5">
          <Link to="/">{appName}</Link>
        </h1>
        <h2>
          Your site was loaded at: {date?.toLocaleString()} {connectionIcon}
        </h2>
        {user && (
          <div className="flex gap-3 items-center">
            <div>
              Hello, <Link to="/profile">{getName(user)}</Link>
            </div>
            <div>
              <button className="btn btn-primary" onClick={logout}>
                Logout
              </button>
            </div>
          </div>
        )}
      </header>
      <main>
        <Outlet />
      </main>
      <footer className="mt-8 text-center">Created with Wasp</footer>

      <label>
        Go to:{' '}
        <select defaultValue="" onChange={(e) => (window.location.href = e.target.value)}>
          <option disabled value="">Select one</option>
          <option value="/">Main page</option>
          <option value="/serialization">Serialization Test</option>
        </select>
      </label>
    </div>
  )
}
