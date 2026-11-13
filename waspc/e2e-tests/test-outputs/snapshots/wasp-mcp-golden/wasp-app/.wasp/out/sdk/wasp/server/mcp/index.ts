import type { McpDescribeResult, McpOperationContext } from '@wasp/lib-mcp/node'
import type { AuthUser } from '../../auth/index.js'
import { config } from '../index.js'
import { routes } from '../../client/router/routes.js'

// PUBLIC API
/**
 * What an operation sees as `context.mcp` when it runs as an MCP tool: the
 * connection's granted scopes and which client it is. It's `undefined` on
 * calls from the web app.
 */
export type { McpCallContext } from '@wasp/lib-mcp/node'

// PUBLIC API
/**
 * Writes what the confirm page shows for a destructive tool call, set with
 * `expose.mcp.destructive: { describe }`. It gets the call's arguments and
 * `{ user, mcp }`, and runs before the user approves, so it should only read.
 *
 * @example
 * export const describeDeleteLink: McpDescribeFn<{ id: string }> = async ({ id }, { user }) => {
 *   const link = await prisma.link.findFirstOrThrow({ where: { id, userId: user.id } })
 *   return { title: `Delete the short link ${link.slug}`, details: link.destinationUrl }
 * }
 */
export type McpDescribeFn<Args = unknown> = (
  args: Args,
  context: McpOperationContext<AuthUser>,
) => McpDescribeResult | Promise<McpDescribeResult>

type Routes = typeof routes

// PUBLIC API
/**
 * An absolute URL to one of the app's routes, so a tool's result can link
 * back into the app. Route names and params are type-checked.
 *
 * @example
 * dashboardUrl('LinkRoute', { params: { id: link.id }, search: { tab: 'stats' } })
 */
export function dashboardUrl<Name extends keyof Routes>(
  routeName: Name,
  ...options: Parameters<Routes[Name]['build']>
): string {
  const build = routes[routeName].build as (...args: unknown[]) => string
  return config.frontendUrl + build(...options)
}
