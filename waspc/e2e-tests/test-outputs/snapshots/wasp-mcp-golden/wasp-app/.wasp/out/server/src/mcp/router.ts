import express from 'express'
import { McpUserError } from '@wasp/lib-mcp/node'
import { HttpError } from 'wasp/server'
import auth from 'wasp/server/core/auth'
import { defineHandler } from 'wasp/server/utils'
import { mcp } from './index.js'

/**
 * The MCP endpoint (`/mcp`), its OAuth routes (`/mcp/oauth/*`) and metadata
 * (`/.well-known/*`). Mounted without the global middleware: they're called
 * with bearer tokens from any origin, and the lib sets CORS and reads the
 * request body itself. Other paths fall through to the next router.
 */
export const mcpRouter = mcp.nodeMiddleware

/**
 * The endpoints `wasp/client/mcp`'s components call (the lib's
 * `McpAppEndpoints`), as the user in their Wasp session.
 */
export const mcpAppRouter = express.Router()
mcpAppRouter.use(auth)
mcpAppRouter.post(
  '/*endpoint',
  defineHandler(async (req, res) => {
    if (!req.user) throw new HttpError(401)
    const endpoint = ([] as string[]).concat(req.params.endpoint ?? []).join('/')
    try {
      res.json(await mcp.user.handle(String(req.user.id), endpoint, req.body))
    } catch (e) {
      if (e instanceof McpUserError) throw new HttpError(e.statusCode, e.message)
      throw e
    }
  }),
)
