import { use, type ReactNode } from 'react'
import { QueryClientProvider } from '@tanstack/react-query'

import { queryClientInitialized } from '../../operations/index'

import { clientAuthHandlers } from '../../auth/schemes'

// Each handler's React context (Clerk's ClerkProvider, for one) wraps the
// whole app, outside the app's own rootComponent slot. Nesting follows
// main.wasp.ts declaration order, first listed outermost -- deterministic,
// and presentation-only: providers are independent identity systems and must
// not depend on each other's context.
const clientAuthHandlerWrappers = Object.values(clientAuthHandlers)
  .map((clientAuthHandler) => clientAuthHandler.Wrapper)
  .filter((Wrapper) => Wrapper !== undefined)

export function WaspApp({ children }: { children: ReactNode }) {
  const queryClient = use(queryClientInitialized)

  return clientAuthHandlerWrappers.reduceRight(
    (wrapped, Wrapper) => <Wrapper>{wrapped}</Wrapper>,
    <QueryClientProvider client={queryClient}>
      {children}
    </QueryClientProvider>,
  )
}
