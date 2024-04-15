import { useSocket } from 'wasp/client/webSocket'
import { Link } from 'wasp/client/router'
import { logout, useAuth } from 'wasp/client/auth'
import { useQuery, getDate } from 'wasp/client/operations'

import './Main.css'
import { getName } from './user'
// Necessary to trigger type tests.
import './testTypes'

export function App({ children }: any) {
  const { data: user } = useAuth()
  const { data: date } = useQuery(getDate)
  const { isConnected } = useSocket()

  const connectionIcon = isConnected ? '🟢' : '🔴'

  const appName = import.meta.env.REACT_APP_NAME ? import.meta.env.REACT_APP_NAME : 'TODO App'

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
      <main>{children}</main>
      <footer className="mt-8 text-center">Created with Wasp</footer>
    </div>
  )
}
