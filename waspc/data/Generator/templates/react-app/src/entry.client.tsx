{{={= =}=}}
import React from 'react'
import ReactDOM from 'react-dom/client'
import { QueryClientProvider } from '@tanstack/react-query'

import { HydratedRouter } from "react-router/dom";
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
  ReactDOM.hydrateRoot(document, (
    <React.StrictMode>
      <QueryClientProvider client={queryClient}>
        {=# areWebSocketsUsed =}
        <WebSocketProvider>
          <HydratedRouter />
        </WebSocketProvider>
        {=/ areWebSocketsUsed =}
        {=^ areWebSocketsUsed =}
        <HydratedRouter />
        {=/ areWebSocketsUsed =}
      </QueryClientProvider>
    </React.StrictMode>
  ))
}
