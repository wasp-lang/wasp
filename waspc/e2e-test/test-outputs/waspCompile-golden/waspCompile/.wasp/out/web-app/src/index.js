import React from 'react'
import ReactDOM from 'react-dom'
import { QueryClientProvider } from 'react-query'

import router from './router'
import { 
  initializeQueryClient,
  queryClientInitialized,
} from './queryClient'
import * as serviceWorker from './serviceWorker'


startApp()

async function startApp() {
  initializeQueryClient()

  await render()

  // If you want your app to work offline and load faster, you can change
  // unregister() to register() below. Note this comes with some pitfalls.
  // Learn more about service workers: https://bit.ly/CRA-PWA
  serviceWorker.unregister()
}

async function render() {
  const queryClient = await queryClientInitialized
  ReactDOM.render(
    <QueryClientProvider client={queryClient}>
      { router }
    </QueryClientProvider>,
    document.getElementById('root')
  )
}
