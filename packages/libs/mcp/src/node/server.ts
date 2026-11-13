// The MCP endpoint. Stateless: a fresh server is built for each request's
// principal, so any instance can answer any request, including a 2026-07-28
// retry. One handler serves both protocol eras.
import {
  createMcpHandler,
  isInputRequiredResult,
  McpServer,
  type AuthInfo,
  type CallToolResult,
  type InputRequiredResult,
  type ServerContext,
} from "@modelcontextprotocol/server";
import type { z } from "zod";
import type { AnyMcpTool, McpLogEntry, McpPrincipal } from "./config";
import type { ConfirmationEnvelope, Confirmations } from "./confirmations";
import { toolErrorResult, toToolError } from "./errors";
import type { Runtime } from "./runtime";
import {
  DESTRUCTIVE_DESCRIPTION_SUFFIX,
  destructiveOutput,
  hasScope,
  isDestructive,
  runTool,
  stepUpScopes,
  whoamiTool,
} from "./tools";

const isPlainObject = (value: unknown): value is Record<string, unknown> =>
  typeof value === "object" && value !== null && !Array.isArray(value);

/**
 * A tool's result as MCP content. A tool with an output schema must return
 * structured content, even an array or nothing (the SDK adapts non-objects
 * for older clients). An operation that returns nothing gives `null`.
 */
function success(result: unknown, hasOutputSchema: boolean): CallToolResult {
  const value = result ?? null;
  return {
    ...((hasOutputSchema || isPlainObject(value)) && {
      structuredContent: value as CallToolResult["structuredContent"],
    }),
    content: [{ type: "text", text: JSON.stringify(value) }],
  };
}

export function authInfoFor(
  rt: Runtime<any>,
  principal: McpPrincipal,
): AuthInfo {
  return {
    token: "",
    clientId: principal.clientId,
    scopes: principal.scopes,
    // Goes into scope challenges, so clients find the authorization server again for step-up.
    resourceMetadataUrl: rt.urls.protectedResourceMetadata,
    extra: { principal },
  };
}

export function createMcpEndpoint<User>(
  rt: Runtime<User>,
  confirmations: Confirmations,
) {
  const builtins: AnyMcpTool<User>[] = [];
  if (rt.config.whoami ?? true) builtins.push(whoamiTool(rt));
  const tools = [...builtins, ...rt.tools.values()];
  const log =
    rt.config.log ??
    ((entry: McpLogEntry) => console.log(JSON.stringify({ mcpTool: entry })));

  function buildServer(principal: McpPrincipal): McpServer {
    const server = new McpServer(
      {
        name: rt.config.name,
        title: rt.config.title,
        version: rt.config.version ?? "1.0.0",
      },
      {
        instructions: rt.config.instructions,
        requestState: { verify: confirmations.verifierFor(principal) },
        // Answering input_required for 2025-era clients needs a session, and this
        // server has none. Those clients never get input_required (they get the link).
        inputRequired: { legacyShim: false },
      },
    );

    for (const tool of tools) {
      const behavior = tool.insufficientScope ?? rt.insufficientScope;
      const allowed = hasScope(rt, principal, tool.scope);
      // Hidden tools aren't registered, so calling one fails like an unknown tool.
      if (!allowed && behavior === "hide") continue;

      const destructive = isDestructive(tool);
      const outputSchema: z.ZodType | undefined = destructive
        ? destructiveOutput(tool.output)
        : tool.output;
      server.registerTool(
        tool.name,
        {
          title: tool.title,
          description: destructive
            ? tool.description + DESTRUCTIVE_DESCRIPTION_SUFFIX
            : tool.description,
          inputSchema: tool.input,
          outputSchema,
          annotations: {
            title: tool.title,
            readOnlyHint: tool.readOnly ?? false,
            destructiveHint: destructive || (tool.destructiveHint ?? false),
            idempotentHint: tool.idempotent ?? false,
            openWorldHint: tool.openWorld ?? false,
          },
          // Missing scope: the endpoint answers 403 insufficient_scope instead of
          // running the tool, and the client asks the user to allow more (step-up).
          scopeChallenge: () =>
            tool.scope && !hasScope(rt, principal, tool.scope)
              ? {
                  scopes: stepUpScopes(rt, principal, tool.scope),
                  // RFC 6750's error-description grammar has no double quotes or backslashes.
                  errorDescription: `${tool.name} needs the ${tool.scope} permission`,
                }
              : undefined,
        },
        async (
          args: Record<string, unknown>,
          ctx: ServerContext,
        ): Promise<CallToolResult | InputRequiredResult> => {
          const started = Date.now();
          let outcome: McpLogEntry["outcome"] = "error";
          try {
            if (destructive && principal.requireConfirmation) {
              const result =
                (await confirmations.resume(tool, principal, ctx)) ??
                (await confirmations.request(tool, args, principal, ctx));
              if (isInputRequiredResult(result)) {
                outcome = "confirmation_required";
                return result;
              }
              if ("error" in result) return toolErrorResult(result.error);
              outcome =
                result.status === "confirmation_required"
                  ? "confirmation_required"
                  : "ok";
              return success(result, true);
            }
            const result = await runTool(rt, tool, args, principal);
            outcome = "ok";
            return success(
              destructive
                ? ({
                    status: "completed",
                    result,
                  } satisfies ConfirmationEnvelope)
                : result,
              outputSchema !== undefined,
            );
          } catch (e) {
            return toolErrorResult(toToolError(e));
          } finally {
            log({
              tool: tool.name,
              userId: principal.userId,
              connectionId: principal.connectionId,
              durationMs: Date.now() - started,
              outcome,
            });
          }
        },
      );
    }
    return server;
  }

  return createMcpHandler(({ authInfo }) => {
    const principal = authInfo?.extra?.principal as McpPrincipal | undefined;
    if (!principal)
      throw new Error(
        "MCP requests must be authenticated before they reach the handler.",
      );
    return buildServer(principal);
  });
}
