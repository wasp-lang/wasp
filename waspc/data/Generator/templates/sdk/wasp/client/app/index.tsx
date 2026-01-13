import { Outlet } from 'react-router-dom'
import { initializeQueryClient } from '../operations'
import { WaspApp, type WaspAppInput } from './components/WaspApp'

const DefaultAppComponent = () => <Outlet />

let isAppInitialized = false

// PRIVATE API (web-app)
export function getWaspApp({
  AppComponent = DefaultAppComponent,
  routesMapping,
}: WaspAppInput): React.ReactNode {
  if (!isAppInitialized) {
    initializeQueryClient()
    isAppInitialized = true
  }

  return <WaspApp AppComponent={AppComponent} routesMapping={routesMapping} />
}

// PRIVATE API (web-app)
export { createAuthRequiredPage } from './pages/createAuthRequiredPage'
