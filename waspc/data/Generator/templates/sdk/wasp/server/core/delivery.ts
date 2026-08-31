{{={= =}=}}
import { configureAppDelivery } from '@wasp.sh/lib-delivery/node'
import config from '../config.js'

export const appDelivery = configureAppDelivery({
  mode: '{= deliveryMode =}',
  serverUrl: config.serverUrl,
  waspApiMountPath: '{= waspApiMountPath =}',
  authEnabled: {= authEnabled =},
  serveClientAssets: {= serveClientAssets =},
})
