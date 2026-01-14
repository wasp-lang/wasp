{{={= =}=}}
import { Outlet } from 'react-router-dom'
import { initializeQueryClient } from '../operations'
import { WaspApp, type WaspAppInput } from './components/WaspApp'

const DefaultRootComponent = () => <Outlet />

let isAppInitialized = false

// PRIVATE API (web-app)
export function getWaspApp({
  RootComponent = DefaultRootComponent,
  routesMapping,
}: WaspAppInput): React.ReactNode {
  if (!isAppInitialized) {
    initializeQueryClient()
    isAppInitialized = true
  }

  return <WaspApp RootComponent={RootComponent} routesMapping={routesMapping} />
}

{=# isAuthEnabled =}
// PRIVATE API (web-app)
export { createAuthRequiredPage } from './pages/createAuthRequiredPage'
{=/ isAuthEnabled =}
