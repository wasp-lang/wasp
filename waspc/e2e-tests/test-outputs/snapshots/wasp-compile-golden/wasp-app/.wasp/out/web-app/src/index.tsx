import React from 'react'
import ReactDOM from 'react-dom/client'
import { QueryClientProvider } from '@tanstack/react-query'

import { router } from './router'
import {
  initializeQueryClient,
  queryClientInitialized,
} from 'wasp/client/operations'



startApp()

async function startApp() {
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
      <QueryClientProvider client={queryClient}>
        {router}
      </QueryClientProvider>
    </React.StrictMode>
  )

  if (rootElement.dataset.waspSsr === '1') {
    ReactDOM.hydrateRoot(rootElement, app)
  } else {
    ReactDOM.createRoot(rootElement).render(app)
  }
}
