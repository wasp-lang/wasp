import { QueryClient, QueryClientProvider } from '@tanstack/react-query'
import { BrowserRouter } from 'react-router'
import type { ReactNode } from 'react'

// PUBLIC API
export function WaspTestWrapper({ children }: { children: ReactNode }) {
  return (
    <QueryClientProvider client={new QueryClient({ defaultOptions: { queries: { retry: false } } })}>
      <BrowserRouter>{children}</BrowserRouter>
    </QueryClientProvider>
  )
}
