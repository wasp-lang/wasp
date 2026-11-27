{{={= =}=}}
// The app's MCP server, from `app.mcp` and the operations with `expose.mcp`.
import {
  createPrismaStore,
  createWaspMcp,
  defineMcpToolFor,
  type McpOperationContext,
} from '@wasp/lib-mcp/node'
import type { AuthUser } from 'wasp/auth'
import { createAuthUserData, makeAuthUserIfPossible } from 'wasp/auth/user'
import { config, env, prisma } from 'wasp/server'
import {
  {=# tools =}
  {= operationName =},
  {=/ tools =}
} from 'wasp/server/operations'
{=# tools =}
{=& input.importStatement =}
{=# output.isDefined =}
{=& output.importStatement =}
{=/ output.isDefined =}
{=# describe.isDefined =}
{=& describe.importStatement =}
{=/ describe.isDefined =}
{=/ tools =}

const defineTool = defineMcpToolFor<AuthUser>()

/** The scopes users can grant a client (`app.mcp.scopes`), in declaration order. */
const mcpScopes = {=& scopesLiteral =}

// An operation as a tool calls it. In the server build, `wasp/server/operations`
// only knows operations as `Payload` in and out (their own types come from the
// user's code), so the tool's schemas can't be checked against them here. The
// schemas move onto the operations in v1 (D4), which brings that check.
type ToolOperation = (
  args: unknown,
  context: McpOperationContext<AuthUser>,
) => Promise<any>

// Tools call their operation through an arrow instead of a direct reference,
// because `wasp/server/operations` and this module can import each other.
const tools = [
  {=# tools =}
  defineTool({
    name: {=& nameLiteral =},
    {=# titleLiteral =}
    title: {=& titleLiteral =},
    {=/ titleLiteral =}
    {=# scopeLiteral =}
    scope: {=& scopeLiteral =},
    {=/ scopeLiteral =}
    {=# insufficientScopeJson =}
    insufficientScope: {=& insufficientScopeJson =},
    {=/ insufficientScopeJson =}
    description: {=& descriptionLiteral =},
    input: {= input.importIdentifier =},
    {=# output.isDefined =}
    output: {= output.importIdentifier =},
    {=/ output.isDefined =}
    readOnly: {= readOnly =},
    idempotent: {= idempotent =},
    openWorld: {= openWorld =},
    // Destructive tools wait for the user's approval on the confirm page.
    {=# describe.isDefined =}
    destructive: {
      // Writes what the confirm page shows.
      describe: (args, context) =>
        {= describe.importIdentifier =}(args, context),
    },
    {=/ describe.isDefined =}
    {=^ describe.isDefined =}
    destructive: {= destructive =},
    {=/ describe.isDefined =}
    operation: (args, context) =>
      ({= operationName =} as unknown as ToolOperation)(args, context),
  }),
  {=/ tools =}
]

async function getUser(userId: string): Promise<AuthUser> {
  const user = await prisma.{= userEntity.prismaIdentifier =}.findUniqueOrThrow({
    where: { id: {=& userIdFromString =} },
    include: { auth: { include: { identities: true } } },
  })
  return makeAuthUserIfPossible(createAuthUserData(user))
}

/** What the built-in whoami tool says about the user: their ID and how they log in. */
function describeUser(user: AuthUser): Record<string, unknown> {
  const identities = Object.fromEntries(
    Object.entries(user.identities)
      .filter(([, identity]) => identity)
      .map(([provider, identity]) => [provider, identity!.id]),
  )
  // JSON can't hold a BigInt id.
  const id = typeof user.id === 'bigint' ? String(user.id) : user.id
  return { id, identities }
}

export const mcp = createWaspMcp<AuthUser>({
  name: {=& appNameLiteral =},
  title: {=& appTitleLiteral =},
  instructions: {=& instructionsLiteral =},
  scopes: mcpScopes,
  {=# initialScopesLiteral =}
  // What clients ask for when they first connect.
  initialScopes: {=& initialScopesLiteral =},
  {=/ initialScopesLiteral =}
  {=# insufficientScopeJson =}
  // 'stepUp' (the default): tools whose scope wasn't granted are listed, and
  // calling one asks the user to allow it. 'hide': they aren't listed.
  insufficientScope: {=& insufficientScopeJson =},
  {=/ insufficientScopeJson =}
  whoami: {= whoami =},
  tools,
  urls: {
    serverUrl: config.serverUrl,
    frontendUrl: config.frontendUrl,
    consentPath: {=& consentPathLiteral =},
    {=# confirmPathLiteral =}
    confirmPath: (actionId) =>
      {=& confirmPathLiteral =} + '?action=' + encodeURIComponent(actionId),
    {=/ confirmPathLiteral =}
  },
  // The McpConnection, McpPendingAction and McpRateLimit models Wasp adds
  // to the Prisma schema. Rate-limit counts are shared across server instances.
  store: createPrismaStore(prisma, {
    userIdToDb: (userId) => {=& userIdFromString =},
  }),
  {=# rateLimitLiteral =}
  rateLimit: {=& rateLimitLiteral =},
  {=/ rateLimitLiteral =}
  getUser,
  describeUser,
  secret: env.{= mcpSecretEnvVarName =},
  allowLoopbackClients: config.isDevelopment,
})
