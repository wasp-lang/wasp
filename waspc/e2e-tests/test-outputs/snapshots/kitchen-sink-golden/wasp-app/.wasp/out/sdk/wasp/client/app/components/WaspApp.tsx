import { use, type ReactNode } from 'react'
import { QueryClientProvider } from '@tanstack/react-query'

import { queryClientPromise } from '../../operations/index'

import { WebSocketProvider } from '../../webSocket/WebSocketProvider'

export function WaspApp({ children }: { children: ReactNode }) {
  const queryClient = use(queryClientPromise)

  return (
    <QueryClientProvider client={queryClient}>
      <WebSocketProvider>
        {children}
      </WebSocketProvider>
    </QueryClientProvider>
  )
}
