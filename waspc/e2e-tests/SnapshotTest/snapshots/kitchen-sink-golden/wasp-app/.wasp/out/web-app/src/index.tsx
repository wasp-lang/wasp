import React from 'react'
import ReactDOM from 'react-dom/client'
import { QueryClientProvider } from '@tanstack/react-query'

import { router } from './router'
import {
  initializeQueryClient,
  queryClientInitialized,
} from 'wasp/client/operations'

import { setup } from '../../../../src/clientSetup'

import { WebSocketProvider } from 'wasp/client/webSocket/WebSocketProvider'

startApp()

async function startApp() {
  await setup()
  initializeQueryClient()

  await render()
}

async function render() {
  const queryClient = await queryClientInitialized
  ReactDOM.createRoot(document.getElementById('root') as HTMLElement).render(
    <React.StrictMode>
      <QueryClientProvider client={queryClient}>
        <WebSocketProvider>
          {router}
        </WebSocketProvider>
      </QueryClientProvider>
    </React.StrictMode>
  )
}
