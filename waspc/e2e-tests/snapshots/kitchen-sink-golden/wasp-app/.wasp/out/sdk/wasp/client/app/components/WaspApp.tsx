import * as React from 'react'
import { QueryClientProvider } from '@tanstack/react-query'

import { getRouter } from '../router/router'
import { queryClientInitialized } from '../../operations/index'

import { WebSocketProvider } from '../../webSocket/WebSocketProvider'

export type WaspAppProps = {
  rootElement?: React.ReactNode;
  routesMapping: Record<string, React.ComponentType>;
}

export function WaspApp({ rootElement, routesMapping }: Required<WaspAppProps>) {
  const [queryClient, setQueryClient] = React.useState<any>(null)

  React.useEffect(() => {
    queryClientInitialized.then(setQueryClient)
  }, [])

  if (!queryClient) {
    return null
  }

  const router = getRouter({
    rootElement,
    routesMapping,
  })

  return (
    <QueryClientProvider client={queryClient}>
      <WebSocketProvider>
        {router}
      </WebSocketProvider>
    </QueryClientProvider>
  )
}
