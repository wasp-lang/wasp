{{={= =}=}}
import { Outlet } from 'react-router'
import { initializeQueryClient } from '../operations'
import { WaspApp, type WaspAppProps } from './components/WaspApp'

const DefaultRootComponent = () => <Outlet />

let isAppInitialized = false

// PRIVATE API (web-app)
export function getWaspApp({
  rootElement = <DefaultRootComponent />,
  routesMapping,
}: WaspAppProps): React.ReactNode {
  if (!isAppInitialized) {
    initializeQueryClient()
    isAppInitialized = true
  }

  return <WaspApp rootElement={rootElement} routesMapping={routesMapping} />
}

{=# isAuthEnabled =}
// PRIVATE API (web-app)
export { createAuthRequiredPage } from './pages/createAuthRequiredPage'
{=/ isAuthEnabled =}
