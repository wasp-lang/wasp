import { configureAppDelivery } from '@wasp.sh/lib-delivery/node'
import config from '../config.js'

export const appDelivery = configureAppDelivery({
  mode: 'integrated',
  serverUrl: config.serverUrl,
  waspApiMountPath: '/api',
  authEnabled: false,
  serveClientAssets: false,
})
