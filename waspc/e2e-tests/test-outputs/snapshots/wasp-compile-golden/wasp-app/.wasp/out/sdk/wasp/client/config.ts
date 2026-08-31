import { stripTrailingSlash } from '../universal/url.js'
import { env } from './env.js'
import { configureBrowserAppDelivery } from '@wasp.sh/lib-delivery/browser'
import { storage } from '../core/storage.js'

const configuredServerUrl = stripTrailingSlash(env["REACT_APP_API_URL"])
export const browserAppDelivery = configureBrowserAppDelivery({
  config: {
    mode: 'integrated',
    serverUrl: configuredServerUrl,
    waspApiMountPath: '/api',
  },
  storage,
})

// PUBLIC API
export type ClientConfig = {
  serverUrl: string,
  /** @deprecated Use serverUrl instead. */
  apiUrl: string,
}

// PUBLIC API
export const config: ClientConfig = {
  serverUrl: browserAppDelivery.serverUrl,
  apiUrl: browserAppDelivery.serverUrl,
}
