{{={= =}=}}
import React from 'react'
import { QueryClientProvider } from '@tanstack/react-query'

import { getRouter } from '../router/router'
import { queryClientInitialized } from '../../operations/index'

{=# areWebSocketsUsed =}
import { WebSocketProvider } from '../../webSocket/WebSocketProvider'
{=/ areWebSocketsUsed =}

export type WaspAppInput = {
  RootComponent?: React.ComponentType;
  routesMapping: Record<string, React.ComponentType>;
}

export function WaspApp({ RootComponent, routesMapping }: Required<WaspAppInput>) {
  const [queryClient, setQueryClient] = React.useState<any>(null)

  React.useEffect(() => {
    queryClientInitialized.then(setQueryClient)
  }, [])

  if (!queryClient) {
    return null
  }

  const router = getRouter({
    RootComponent,
    routesMapping,
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
