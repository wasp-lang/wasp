{{={= =}=}}
import React from 'react'
import ReactDOM from 'react-dom'
import { QueryClientProvider } from 'react-query'

import router from './router'
import { 
  queryClientInitialized,
  {=^ doesClientSetupFnExist =}
  setupQueryClient,
  {=/ doesClientSetupFnExist =}
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
  {=^ doesClientSetupFnExist =}
  setupQueryClient()
  {=/ doesClientSetupFnExist =}

  render()

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
