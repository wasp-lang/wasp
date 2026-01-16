import React from 'react'
import { QueryClientProvider } from '@tanstack/react-query'

import { getRouter } from '../router/router'
import { queryClientInitialized } from '../../operations/index'


export type WaspAppProps = {
  RootComponent?: React.ComponentType;
  routesMapping: Record<string, React.ComponentType>;
}

export function WaspApp({ RootComponent, routesMapping }: Required<WaspAppProps>) {
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
      {router}
    </QueryClientProvider>
  )
}
