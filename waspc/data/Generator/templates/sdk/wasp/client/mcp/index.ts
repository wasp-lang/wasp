{{={= =}=}}
import type { McpAppCall } from '@wasp/lib-mcp'
import { createMcpComponents } from '@wasp/lib-mcp/browser'
import { api, handleApiError } from '../../api/index.js'
import { config } from '../config.js'
import { clearMcpReturnTo } from './returnTo.js'

// PUBLIC API (experimental)
/** The URL users paste into an AI client to connect it to this app. */
export const mcpServerUrl = `${config.apiUrl}/mcp`

const call: McpAppCall = async (endpoint, body) => {
  try {
    return await api.post(`/{= mcpAppRouteInRootRouter =}/${endpoint}`, { json: body }).json()
  } catch (e) {
    throw handleApiError(e)
  }
}

// PUBLIC API (experimental)
/**
 * - `McpConsentForm`: the consent screen, on the page `app.mcp.consentRoute` points to.
 * - `McpConnections`: the user's connected AI apps, and the URL to connect one.
 * - `McpConfirmAction`: approving a destructive tool call, on the page
 *   `app.mcp.confirmRoute` points to.
 *
 * They take the same `logo` and `appearance` as the auth forms.
 */
export const { McpConsentForm, McpConnections, McpConfirmAction } = createMcpComponents({
  call,
  mcpServerUrl,
  hasDestructiveTools: {= hasDestructiveTools =},
  // The user made it to the page, so the return-after-login is used up.
  onPageOpened: clearMcpReturnTo,
})

// PUBLIC API (experimental)
export type { McpCustomizationOptions } from '@wasp/lib-mcp/browser'
