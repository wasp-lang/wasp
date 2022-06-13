{{={= =}=}}
import React from 'react'
import ReactDOM from 'react-dom'
import { QueryClientProvider } from 'react-query'
import api from './api.js'
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

  await setCsrfToken()

  await render()

  // If you want your app to work offline and load faster, you can change
  // unregister() to register() below. Note this comes with some pitfalls.
  // Learn more about service workers: https://bit.ly/CRA-PWA
  serviceWorker.unregister()
}

// TODO: Chat on options. Pretty hacky.
async function setCsrfToken() {
  const token = await api.get(config.apiUrl + '/csrf-token')

  const meta = document.createElement('meta')
  meta.name = "csrf-token"
  meta.content = token.data
  document.getElementsByTagName('head')[0].appendChild(meta)
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
