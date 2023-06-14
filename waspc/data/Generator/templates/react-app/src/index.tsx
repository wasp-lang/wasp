{{={= =}=}}
import React from 'react'
import ReactDOM from 'react-dom/client'
import { QueryClientProvider } from '@tanstack/react-query'

import router from './router'
import {
  initializeQueryClient,
  queryClientInitialized,
} from './queryClient'

{=# setupFn.isDefined =}
{=& setupFn.importStatement =}
{=/ setupFn.isDefined =}

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
  ReactDOM.createRoot(document.getElementById('root') as HTMLElement).render(
    <React.StrictMode>
      <QueryClientProvider client={queryClient}>
        {router}
      </QueryClientProvider>
    </React.StrictMode>
  )
}
