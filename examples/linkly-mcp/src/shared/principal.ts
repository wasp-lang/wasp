import { DomainError } from "./errors";
import { ALL_SCOPES, isScope, type Scope } from "./scopes";

// Every service call takes a Principal, regardless of which adapter it came from.
export type Principal = {
  userId: string;
  scopes: Set<Scope>;
  source: "session" | "mcp";
  clientName?: string; // mcp only
};

/** Dashboard sessions get every scope: the human is already in the product. */
export function principalFromSession(user: { id: string }): Principal {
  return { userId: user.id, scopes: new Set(ALL_SCOPES), source: "session" };
}

/** MCP tools run with the scopes the user granted the connection (implied scopes included). */
export function principalFromMcp(ctx: {
  user: { id: string };
  scopes: string[];
  connection: { clientName: string };
}): Principal {
  return {
    userId: ctx.user.id,
    scopes: new Set(ctx.scopes.filter(isScope)),
    source: "mcp",
    clientName: ctx.connection.clientName,
  };
}

export function hasScope(p: Principal, scope: Scope): boolean {
  return p.scopes.has(scope);
}

export function requireScope(p: Principal, scope: Scope): void {
  if (!p.scopes.has(scope)) {
    throw new DomainError(
      "forbidden",
      `This action needs the "${scope}" permission, which this ${describeSource(p)} doesn't have.`,
      { requiredScope: scope },
    );
  }
}

export function requireSession(p: Principal, what: string): void {
  if (p.source !== "session") {
    throw new DomainError(
      "forbidden",
      `${what} can only be done from the Linkly dashboard.`,
    );
  }
}

function describeSource(p: Principal): string {
  if (p.source === "mcp") return `connection (${p.clientName ?? "app"})`;
  return "session";
}
