// What the user-facing functions return to the consent, connections, and
// confirm pages. Plain JSON, so they can travel through Wasp operations.

/** Typed as JSON, not `unknown`: Wasp operations only accept serializable payloads. */
export type JsonValue =
  | string
  | number
  | boolean
  | null
  | JsonValue[]
  | { [key: string]: JsonValue };

export type AuthorizeParams = {
  response_type?: string;
  client_id?: string;
  redirect_uri?: string;
  code_challenge?: string;
  code_challenge_method?: string;
  state?: string;
  scope?: string;
  resource?: string;
};

export type ScopeView = {
  scope: string;
  label: string;
  description?: string;
  implies: string[];
};

/** A valid request: what the consent page shows. */
export type AuthorizeRequestView = {
  ok: true;
  clientId: string;
  clientName: string;
  /** The domain the user is really trusting: the host of the client's metadata URL. */
  clientHost: string;
  /** Every scope the app declares, for the checkboxes. */
  scopes: ScopeView[];
  /** What the client asked for (pre-checked). */
  requestedScopes: string[];
  /** This user's newest connection to the client, if any: re-consent starts from it. */
  existingConnection: {
    scopes: string[];
    requireConfirmation: boolean;
  } | null;
};

/** A problem the client should hear about: the page sends the browser to `redirectTo`. */
export type RedirectableAuthorizeError = {
  ok: false;
  redirectTo: string;
  error: string;
  errorDescription: string;
};

/** A connected client, with all its connections (one per authorization) as one. */
export type ConnectionView = {
  clientId: string;
  clientName: string;
  clientHost: string;
  scopes: string[];
  /** The most the user can grant: what the client asked for. */
  requestedScopes: string[];
  requireConfirmation: boolean;
  /** When it was first connected. */
  createdAt: string;
  lastUsedAt: string | null;
};

export type PendingActionStatus = "pending" | "approved" | "denied" | "expired";

export type ToolErrorView = {
  code: string;
  message: string;
  nextStep?: string;
};

export type PendingActionView = {
  id: string;
  status: PendingActionStatus;
  toolName: string;
  toolTitle: string;
  summary: { title: string; details?: string };
  args: JsonValue;
  clientName: string;
  clientHost: string;
  expiresAt: string;
  /** Set once the user approved and the tool ran. */
  outcome:
    | { ok: true; result: JsonValue }
    | { ok: false; error: ToolErrorView }
    | null;
};
