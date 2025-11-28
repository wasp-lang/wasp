{{={= =}=}}
import React from 'react'
import { QueryClientProvider } from '@tanstack/react-query'

import { router } from '../router/router'
import {
  initializeQueryClient,
  queryClientInitialized,
} from '../operations'

{=# setupFn.isDefined =}
{=& setupFn.importStatement =}
{=/ setupFn.isDefined =}

{=# areWebSocketsUsed =}
import { WebSocketProvider } from '../webSocket/WebSocketProvider'
{=/ areWebSocketsUsed =}

let isAppInitialized = false

export function WaspApp() {
  if (!isAppInitialized) {
    startApp()
    isAppInitialized = true
  }

  return <WaspAppContent />
}

function startApp() {
  {=# setupFn.isDefined =}
  // Run setup function if defined (non-blocking)
  Promise.resolve({= setupFn.importIdentifier =}()).catch((error) => {
    console.error('Error in setup function:', error)
  })
  {=/ setupFn.isDefined =}
  initializeQueryClient()
}

function WaspAppContent() {
  const [queryClient, setQueryClient] = React.useState<any>(null)

  React.useEffect(() => {
    queryClientInitialized.then(setQueryClient)
  }, [])

  if (!queryClient) {
    return null
  }

  return (
    <QueryClientProvider client={queryClient}>
      {=# areWebSocketsUsed =}
      <WebSocketProvider>
        {router}
      </WebSocketProvider>
      {=/ areWebSocketsUsed =}
      {=^ areWebSocketsUsed =}
      {router}
      {=/ areWebSocketsUsed =}
    </QueryClientProvider>
  )
}
