{{={= =}=}}
import { use, type ReactNode } from 'react'
import { QueryClientProvider } from '@tanstack/react-query'

import { queryClientPromise } from '../../operations/index'

{=# areWebSocketsUsed =}
import { WebSocketProvider } from '../../webSocket/WebSocketProvider'
{=/ areWebSocketsUsed =}

export function WaspApp({ children }: { children: ReactNode }) {
  const queryClient = use(queryClientPromise)

  return (
    <QueryClientProvider client={queryClient}>
      {=# areWebSocketsUsed =}
      <WebSocketProvider>
        {children}
      </WebSocketProvider>
      {=/ areWebSocketsUsed =}
      {=^ areWebSocketsUsed =}
      {children}
      {=/ areWebSocketsUsed =}
    </QueryClientProvider>
  )
}
