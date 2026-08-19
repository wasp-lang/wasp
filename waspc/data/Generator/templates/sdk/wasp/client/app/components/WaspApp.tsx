{{={= =}=}}
import { use, type ReactNode } from 'react'
import { QueryClientProvider } from '@tanstack/react-query'

import { queryClientInitialized } from '../../operations/index'

{=# areWebSocketsUsed =}
import { WebSocketProvider } from '../../webSocket/WebSocketProvider'
{=/ areWebSocketsUsed =}
{=# isClientAuthAdapterUsed =}
import { Fragment } from 'react'

import { clientAuthAdapter } from '../../auth/provider'

// The provider's React context (Clerk's ClerkProvider, for one) wraps the whole
// app, outside the app's own rootComponent slot.
const ClientAuthAdapterWrapper = clientAuthAdapter.Wrapper ?? Fragment
{=/ isClientAuthAdapterUsed =}

export function WaspApp({ children }: { children: ReactNode }) {
  const queryClient = use(queryClientInitialized)

  return (
    {=# isClientAuthAdapterUsed =}
    <ClientAuthAdapterWrapper>
    {=/ isClientAuthAdapterUsed =}
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
    {=# isClientAuthAdapterUsed =}
    </ClientAuthAdapterWrapper>
    {=/ isClientAuthAdapterUsed =}
  )
}
