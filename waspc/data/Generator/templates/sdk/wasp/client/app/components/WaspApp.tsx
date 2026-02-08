{{={= =}=}}
import * as React from 'react'
import { QueryClientProvider } from '@tanstack/react-query'

import { getRouter } from '../router/router'
import { queryClientInitialized, getQueryClientSync } from '../../operations/index'

{=# areWebSocketsUsed =}
import { WebSocketProvider } from '../../webSocket/WebSocketProvider'
{=/ areWebSocketsUsed =}

export type WaspAppProps = {
  rootElement?: React.ReactNode;
  routesMapping: Record<string, React.ComponentType>;
  routeNameToSsr?: Record<string, boolean>;
}

export function WaspApp({
  rootElement,
  routesMapping,
  routeNameToSsr,
}: Required<WaspAppProps>) {
  // Use synchronous getter as initial value so the first client render matches
  // the server-rendered HTML during SSR hydration (avoids returning null and
  // causing a hydration mismatch / double-tree).
  const [queryClient, setQueryClient] = React.useState<any>(() => getQueryClientSync())

  React.useEffect(() => {
    if (!queryClient) {
      queryClientInitialized.then(setQueryClient)
    }
  }, [queryClient])

  if (!queryClient) {
    return null
  }

  const router = getRouter({
    rootElement,
    routesMapping,
    routeNameToSsr,
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
