import { use, type ReactNode } from 'react'
import { QueryClientProvider } from '@tanstack/react-query'

import { queryClientInitialized } from '../../operations/index'

import { Fragment } from 'react'

import { clientAuthAdapter } from '../../auth/provider'

// The provider's React context (Clerk's ClerkProvider, for one) wraps the whole
// app, outside the app's own rootComponent slot.
const ClientAuthAdapterWrapper = clientAuthAdapter.Wrapper ?? Fragment

export function WaspApp({ children }: { children: ReactNode }) {
  const queryClient = use(queryClientInitialized)

  return (
    <ClientAuthAdapterWrapper>
    <QueryClientProvider client={queryClient}>
      {children}
    </QueryClientProvider>
    </ClientAuthAdapterWrapper>
  )
}
