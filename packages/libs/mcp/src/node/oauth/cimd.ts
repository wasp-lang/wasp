// Client ID Metadata Documents: the OAuth `client_id` is an HTTPS URL that
// serves a JSON description of the client. Any client with a valid document
// can connect: there is no allowlist. The consent screen is the boundary.
import { McpUserError } from "../errors";
import type { Runtime } from "../runtime";
import { fetchJsonGuarded, LOOPBACK_HOSTNAMES, SsrfError } from "./ssrf";

export type ClientMetadata = {
  clientId: string;
  clientName: string;
  clientHost: string;
  redirectUris: string[];
};

const CACHE_TTL_MS = 24 * 60 * 60 * 1000;
/** Anyone can send a client_id, so the cache drops its oldest entry past this size. */
const CACHE_MAX_ENTRIES = 1000;

/** Errors about the client itself: the consent page shows them and never redirects. */
export class InvalidClientError extends McpUserError {
  constructor(message: string) {
    super(400, message);
    this.name = "InvalidClientError";
  }
}

/** `client_id` must be an https URL with a path (or loopback http, when allowed). */
export function parseClientIdUrl(
  clientId: string,
  allowLoopback: boolean,
): URL {
  let url: URL;
  try {
    url = new URL(clientId);
  } catch {
    throw new InvalidClientError(
      "client_id must be a URL pointing to the client's metadata document.",
    );
  }
  const loopback = LOOPBACK_HOSTNAMES.has(url.hostname);
  if (loopback && !allowLoopback)
    throw new InvalidClientError("client_id can't point at localhost.");
  if (url.protocol !== "https:" && !(url.protocol === "http:" && loopback)) {
    throw new InvalidClientError("client_id must be an https:// URL.");
  }
  if (url.pathname === "/" || url.pathname === "")
    throw new InvalidClientError("client_id URL must include a path.");
  if (url.hash || url.username || url.password) {
    throw new InvalidClientError(
      "client_id URL must not contain credentials or a fragment.",
    );
  }
  return url;
}

export function validateMetadataDocument(
  clientId: string,
  doc: unknown,
): ClientMetadata {
  if (!doc || typeof doc !== "object")
    throw new InvalidClientError(
      "The client metadata document must be a JSON object.",
    );
  const d = doc as Record<string, unknown>;
  if (d.client_id !== clientId) {
    throw new InvalidClientError(
      "The client metadata document's client_id doesn't match the URL it was fetched from.",
    );
  }
  const redirectUris = d.redirect_uris;
  if (
    !Array.isArray(redirectUris) ||
    redirectUris.length === 0 ||
    !redirectUris.every((u) => typeof u === "string")
  ) {
    throw new InvalidClientError(
      "The client metadata document must list at least one redirect_uris entry.",
    );
  }
  for (const uri of redirectUris as string[]) {
    let parsed: URL;
    try {
      parsed = new URL(uri);
    } catch {
      throw new InvalidClientError(
        `Invalid redirect URI in client metadata: ${uri}`,
      );
    }
    if (parsed.hash)
      throw new InvalidClientError(
        "Redirect URIs must not contain a fragment.",
      );
    // The consent page sends the browser there, so a javascript: or data: URI
    // would run on the app's origin. MCP allows https, and http on loopback.
    if (
      parsed.protocol !== "https:" &&
      !(parsed.protocol === "http:" && isLoopbackHost(parsed.hostname))
    )
      throw new InvalidClientError(
        `Redirect URIs must use https (or http on localhost): ${uri}`,
      );
  }
  const host = new URL(clientId).hostname;
  const name =
    typeof d.client_name === "string" && d.client_name.trim()
      ? d.client_name.trim().slice(0, 100)
      : host;
  return {
    clientId,
    clientName: name,
    clientHost: host,
    redirectUris: redirectUris as string[],
  };
}

const isLoopbackHost = (hostname: string) =>
  hostname === "127.0.0.1" || hostname === "localhost" || hostname === "[::1]";

/** Exact match, except loopback redirects match on any port (RFC 8252 §7.3). */
export function redirectUriMatches(
  requested: string,
  registered: string[],
): boolean {
  if (registered.includes(requested)) return true;
  let req: URL;
  try {
    req = new URL(requested);
  } catch {
    return false;
  }
  if (req.protocol !== "http:" || !isLoopbackHost(req.hostname)) return false;
  return registered.some((r) => {
    try {
      const reg = new URL(r);
      return (
        reg.protocol === "http:" &&
        reg.hostname === req.hostname &&
        reg.pathname === req.pathname &&
        reg.search === req.search
      );
    } catch {
      return false;
    }
  });
}

/** Resolves a client: from the 24h cache, else fetched with SSRF guards, then validated. */
export async function resolveClient(
  rt: Runtime,
  clientId: string,
): Promise<ClientMetadata> {
  const allowLoopback = rt.config.allowLoopbackClients ?? false;
  const url = parseClientIdUrl(clientId, allowLoopback);

  const cached = rt.clientCache.get(clientId);
  if (
    cached &&
    rt.now().getTime() - cached.fetchedAt.getTime() < CACHE_TTL_MS
  ) {
    return validateMetadataDocument(clientId, cached.metadata);
  }

  const fetchDocument =
    rt.config.fetchClientMetadata ??
    ((u: URL) => fetchJsonGuarded(u, { allowLoopback }));
  let doc: unknown;
  try {
    doc = await fetchDocument(url);
  } catch (e) {
    if (e instanceof SsrfError) throw new InvalidClientError(e.message);
    throw new InvalidClientError(
      `Couldn't fetch the client metadata document: ${(e as Error).message}`,
    );
  }
  const metadata = validateMetadataDocument(clientId, doc);
  rt.clientCache.delete(clientId);
  if (rt.clientCache.size >= CACHE_MAX_ENTRIES) {
    const oldest = rt.clientCache.keys().next().value;
    if (oldest !== undefined) rt.clientCache.delete(oldest);
  }
  rt.clientCache.set(clientId, { metadata: doc, fetchedAt: rt.now() });
  return metadata;
}
