import React from 'react'
import ReactDOM from 'react-dom'
import { QueryClientProvider } from '@tanstack/react-query'

import router from './router'
import {
  initializeQueryClient,
  queryClientInitialized,
} from './queryClient'

import myClientSetupFunction from './ext-src/myClientSetupCode.js'

startApp()

async function startApp() {
  await myClientSetupFunction()
  initializeQueryClient()

  await render()
}

async function render() {
  const queryClient = await queryClientInitialized
  ReactDOM.render(
    <QueryClientProvider client={queryClient}>
      {router}
    </QueryClientProvider>,
    document.getElementById('root')
  )
}
