{{={= =}=}}
import React from 'react'
import ReactDOM from 'react-dom'
import { QueryClientProvider } from '@tanstack/react-query'

import router from './router'
import {
  initializeQueryClient,
  queryClientInitialized,
} from './queryClient'
import * as serviceWorker from './serviceWorker'

{=# setupFn.isDefined =}
{=& setupFn.importStatement =}
{=/ setupFn.isDefined =}
{=# rootComponent.isDefined =}
{=& rootComponent.importStatement =}
{=/ rootComponent.isDefined =}

startApp()

async function startApp() {
  {=# setupFn.isDefined =}
  await {= setupFn.importIdentifier =}()
  {=/ setupFn.isDefined =}
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
      {=# rootComponent.isDefined =}
      <{= rootComponent.importIdentifier =}>
      {=/ rootComponent.isDefined =}
      {router}
      {=# rootComponent.isDefined =}
      </{= rootComponent.importIdentifier =}>
      {=/ rootComponent.isDefined =}
    </QueryClientProvider>,
    document.getElementById('root')
  )
}
