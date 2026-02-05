{{={= =}=}}
import React from 'react'
import ReactDOM from 'react-dom/client'
import { QueryClientProvider } from '@tanstack/react-query'
import { HelmetProvider } from 'react-helmet-async'

import { router } from './router'
import {
  initializeQueryClient,
  queryClientInitialized,
} from 'wasp/client/operations'

{=# setupFn.isDefined =}
{=& setupFn.importStatement =}
{=/ setupFn.isDefined =}

{=# areWebSocketsUsed =}
import { WebSocketProvider } from 'wasp/client/webSocket/WebSocketProvider'
{=/ areWebSocketsUsed =}

startApp()

async function startApp() {
  {=# setupFn.isDefined =}
  await {= setupFn.importIdentifier =}()
  {=/ setupFn.isDefined =}
  initializeQueryClient()

  await render()
}

async function render() {
  const queryClient = await queryClientInitialized
  const rootElement = document.getElementById('root')
  if (!rootElement) {
    throw new Error('Root element not found')
  }
  const app = (
    <React.StrictMode>
      <HelmetProvider>
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
      </HelmetProvider>
    </React.StrictMode>
  )

  if (rootElement.dataset.waspSsr === '1') {
    ReactDOM.hydrateRoot(rootElement, app)
  } else {
    ReactDOM.createRoot(rootElement).render(app)
  }
}
