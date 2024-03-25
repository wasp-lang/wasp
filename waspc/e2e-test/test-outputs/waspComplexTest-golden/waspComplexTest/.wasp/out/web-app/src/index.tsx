import React from 'react'
import ReactDOM from 'react-dom/client'
import { QueryClientProvider } from '@tanstack/react-query'

import router from './router'
import {
  initializeQueryClient,
  queryClientInitialized,
} from 'wasp/client/operations'

import myClientSetupFunction__userDefined from '../../../../src/client/myClientSetupCode.js'


startApp()

async function startApp() {
  await myClientSetupFunction__userDefined()
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
