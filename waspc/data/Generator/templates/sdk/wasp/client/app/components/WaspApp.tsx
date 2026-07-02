{{={= =}=}}
import { type ReactNode } from 'react'
import { QueryClientProvider } from '@tanstack/react-query'

import { queryClient } from '../client-setup'

{=# areWebSocketsUsed =}
import { WebSocketProvider } from '../../webSocket/WebSocketProvider'
{=/ areWebSocketsUsed =}

export function WaspApp({ children }: { children: ReactNode }) {
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
