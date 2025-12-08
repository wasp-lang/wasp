{{={= =}=}}
import React from 'react'
import { QueryClientProvider } from '@tanstack/react-query'
import { Outlet } from 'react-router-dom'

import { getRouter } from '../router/router'
import {
  initializeQueryClient,
  queryClientInitialized,
} from '../operations'

{=# areWebSocketsUsed =}
import { WebSocketProvider } from '../webSocket/WebSocketProvider'
{=/ areWebSocketsUsed =}

let isAppInitialized = false

const DefaultAppComponent = () => <Outlet />

type WaspAppInput = {
  AppComponent?: React.ComponentType;
  routes: Record<string, React.ComponentType>;
}

export function getWaspApp({
  AppComponent = DefaultAppComponent,
  routes,
}: WaspAppInput): React.ReactNode {
  if (!isAppInitialized) {
    initializeQueryClient()
    isAppInitialized = true
  }

  return <WaspApp AppComponent={AppComponent} routes={routes} />
}

function WaspApp({ AppComponent, routes }: Required<WaspAppInput>) {
  const [queryClient, setQueryClient] = React.useState<any>(null)

  React.useEffect(() => {
    queryClientInitialized.then(setQueryClient)
  }, [])

  if (!queryClient) {
    return null
  }

  const router = getRouter({
    AppComponent,
    routesMapping: routes,
  })

  return (
    <QueryClientProvider client={queryClient}>
      {=# areWebSocketsUsed =}
      <WebSocketProvider>
        {router}
      </WebSocketProvider>
      {=/ areWebSocketsUsed =}
      {=^ areWebSocketsUsed =}
      {router}
      {=/ areWebSocketsUsed =}
    </QueryClientProvider>
  )
}
