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
  {=# doesClientSetupFnExist =}
  await {= clientSetupJsFnIdentifier =}()
  {=/ doesClientSetupFnExist =}
  initializeQueryClient()

  await setCsrfTokenHeader()

  await render()

  // If you want your app to work offline and load faster, you can change
  // unregister() to register() below. Note this comes with some pitfalls.
  // Learn more about service workers: https://bit.ly/CRA-PWA
  serviceWorker.unregister()
}

// NOTE: Since users will likely have the backend running on a different domain than
// the frontend, we are unable to set the token:
// (a) on the page load, as the index.html is not served by Node, nor
// (b) via a cookie, since the frontend JS will not be able to access a cross-domain cookie.
async function setCsrfTokenHeader() {
  try {
    const token = await api.get(config.apiUrl + '/csrf-token')
    if (token.data) {
      api.defaults.headers.common['X-CSRF-Token'] = token.data
    }
  } catch (error) {
    handleApiError(error)
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
