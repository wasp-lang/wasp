import { use, type ReactNode } from 'react'
import { QueryClientProvider } from '@tanstack/react-query'

import { queryClientInitialized } from '../../operations/index'

import { WebSocketProvider } from '../../webSocket/WebSocketProvider'

export function WaspApp({ children }: { children: ReactNode }) {
  const queryClient = use(queryClientInitialized)

  return (
    <QueryClientProvider client={queryClient}>
      <WebSocketProvider>
        {children}
      </WebSocketProvider>
    </QueryClientProvider>
  )
}
