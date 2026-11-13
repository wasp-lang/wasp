// Tool errors are written for agents: a code to branch on, a message, and
// what to do next. Anything thrown by a tool goes through toToolError, so a
// Wasp HttpError thrown by an operation reads the same as one thrown here.
import type { CallToolResult } from "@modelcontextprotocol/server";
import type { ToolErrorView } from "../views";

/** Throw from a tool to give the agent a specific code and next step. */
export class McpToolError extends Error {
  constructor(
    public readonly code: string,
    message: string,
    public readonly nextStep?: string,
  ) {
    super(message);
    this.name = "McpToolError";
  }
}

/**
 * Thrown by the user-facing functions (consent, connections, confirmations).
 * Has the same shape as Wasp's HttpError, so adapters can rethrow it as one.
 */
export class McpUserError extends Error {
  constructor(
    public readonly statusCode: number,
    message: string,
  ) {
    super(message);
    this.name = "McpUserError";
  }
}

const CODE_BY_STATUS: Record<number, string> = {
  400: "invalid_input",
  401: "unauthorized",
  403: "forbidden",
  404: "not_found",
  409: "conflict",
  422: "invalid_input",
  429: "rate_limited",
};

type HttpErrorLike = {
  statusCode: number;
  message: string;
  data?: unknown;
};

function isHttpErrorLike(e: unknown): e is HttpErrorLike {
  return (
    e instanceof Error &&
    typeof (e as { statusCode?: unknown }).statusCode === "number"
  );
}

type ZodErrorLike = {
  name: string;
  issues: { path: PropertyKey[]; message: string }[];
};

function isZodErrorLike(e: unknown): e is ZodErrorLike {
  return (
    typeof e === "object" &&
    e !== null &&
    (e as { name?: unknown }).name === "ZodError" &&
    Array.isArray((e as { issues?: unknown }).issues)
  );
}

function stringField(data: unknown, key: string): string | undefined {
  if (typeof data !== "object" || data === null) return undefined;
  const value = (data as Record<string, unknown>)[key];
  return typeof value === "string" ? value : undefined;
}

/**
 * Normalizes anything a tool throws. Unknown errors become `internal` with a
 * generic message, so stack traces and SQL never reach the model; they're logged instead.
 */
export function toToolError(e: unknown): ToolErrorView {
  if (e instanceof McpToolError) {
    return { code: e.code, message: e.message, nextStep: e.nextStep };
  }
  if (isZodErrorLike(e)) {
    const summary = e.issues
      .map((i) => `${i.path.length ? i.path.join(".") : "input"}: ${i.message}`)
      .join("; ");
    return {
      code: "invalid_input",
      message: `Invalid input. ${summary}`,
      nextStep: "Fix the listed fields and call the tool again.",
    };
  }
  if (isHttpErrorLike(e) && e.statusCode < 500) {
    return {
      code:
        stringField(e.data, "code") ?? CODE_BY_STATUS[e.statusCode] ?? "error",
      message: e.message,
      nextStep: stringField(e.data, "nextStep"),
    };
  }
  console.error(e);
  return {
    code: "internal",
    message: "Something went wrong on the server.",
    nextStep: "Try again. If it keeps failing, tell the user.",
  };
}

function toolErrorText(error: ToolErrorView): string {
  return `${error.code}: ${error.message}${error.nextStep ? ` ${error.nextStep}` : ""}`;
}

export function toolErrorResult(error: ToolErrorView): CallToolResult {
  return {
    isError: true,
    content: [{ type: "text", text: toolErrorText(error) }],
  };
}
