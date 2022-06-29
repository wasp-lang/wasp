{{={= =}=}}
import React from 'react'
import ReactDOM from 'react-dom'
import { QueryClientProvider } from 'react-query'
import api, { handleApiError } from './api.js'
import config from './config.js'

import router from './router'
import { 
  initializeQueryClient,
  queryClientInitialized,
} from './queryClient'
import * as serviceWorker from './serviceWorker'

{=# doesClientSetupFnExist =}
{=& clientSetupJsFnImportStatement =}
{=/ doesClientSetupFnExist =}

import './index.css'

startApp()

async function startApp() {
  await obtainCsrfTokenHeader()

  {=# doesClientSetupFnExist =}
  await {= clientSetupJsFnIdentifier =}()
  {=/ doesClientSetupFnExist =}
  initializeQueryClient()

  await render()

  // If you want your app to work offline and load faster, you can change
  // unregister() to register() below. Note this comes with some pitfalls.
  // Learn more about service workers: https://bit.ly/CRA-PWA
  serviceWorker.unregister()
}

// NOTE: Gets token from iframe, which also handles local logout.
async function obtainCsrfTokenHeader() {
  try {
    const token = await window.csrfToken
    api.defaults.headers.common['X-CSRF-Token'] = token
  } catch (e) {
    // TODO: render the failed page
  }
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
