import { WaspAuthClientError, post } from "./http.js";
import { getClientRuntime, getClientSpec } from "./runtime.js";
import type { OAuthProviderName } from "./types.js";

/** The server path prefix the routes live under. */
function basePath(): string {
  return getClientRuntime().mountUrl;
}

/**
 * Adopts what the server answered a login with: a bearer credential in the
 * body, which the generated client stores and attaches to every request; or
 * nothing, when the credential travels as a cookie the browser already holds.
 * Either way the cached queries are refreshed, so `useAuth()` sees the login.
 */
async function adoptSignIn(body: SignInAnswer): Promise<void> {
  await getClientRuntime().setCredential(
    typeof body.credential === "string" ? body.credential : null,
    { persistent: body.persistent !== false },
  );
}

type SignInAnswer = { credential?: unknown; persistent?: unknown };

// PUBLIC API
/**
 * `persistent: false` is a login without "remember me": the credential lasts
 * for the browser session only.
 */
export async function login(
  data: (
    | { username: string; password: string }
    | { email: string; password: string }
  ) & { persistent?: boolean },
): Promise<void> {
  const path =
    "email" in data
      ? `${basePath()}/email/login`
      : `${basePath()}/username/login`;
  await adoptSignIn(await post<SignInAnswer>(path, data));
}

// PUBLIC API
export async function signup(
  data: (
    | { username: string; password: string }
    | { email: string; password: string }
  ) &
    Record<string, unknown>,
): Promise<{ success: boolean }> {
  const path =
    "email" in data
      ? `${basePath()}/email/signup`
      : `${basePath()}/username/signup`;
  const result = await post<{ success?: boolean }>(path, data);
  return { success: result.success ?? true };
}

// PUBLIC API
export async function requestPasswordReset(data: {
  email: string;
}): Promise<{ success: boolean }> {
  const { success } = await post<{ success: boolean }>(
    `${basePath()}/email/request-password-reset`,
    data,
  );
  return { success };
}

// PUBLIC API
export async function resetPassword(data: {
  token: string;
  password: string;
}): Promise<{ success: boolean }> {
  const { success } = await post<{ success: boolean }>(
    `${basePath()}/email/reset-password`,
    data,
  );
  return { success };
}

// PUBLIC API
export async function verifyEmail(data: {
  token: string;
}): Promise<{ success: boolean; reason?: string }> {
  return post<{ success: boolean; reason?: string }>(
    `${basePath()}/email/verify-email`,
    data,
  );
}

// PRIVATE API
export async function exchangeOAuthCodeForSession(code: string): Promise<void> {
  await adoptSignIn(
    await post<SignInAnswer>(`${basePath()}/exchange-code`, {
      code,
    }),
  );
}

export function signInUrl(provider: OAuthProviderName): string {
  return `${basePath()}/${provider}/login`;
}

export function isMethodEnabled(
  name: keyof ReturnType<typeof getClientSpec>["methods"],
): boolean {
  return getClientSpec().methods[name] !== undefined;
}

// --- account linking --------------------------------------------------------

type LinkedIdentity = { providerName: string; providerUserId: string };

/**
 * `merge-required`: the login belongs to another account, and you just
 * proved it is yours. Ask the user, then pass the ticket to
 * {@link confirmMerge}. Only apps that declare `auth.mergeUsers` ever see it.
 */
export type LinkResult =
  | { status: "linked" }
  | { status: "merge-required"; mergeTicket: string };

async function linkThrough(path: string, data: unknown): Promise<LinkResult> {
  try {
    await post(`${basePath()}${path}`, data);
  } catch (e) {
    const mergeTicket = getMergeTicket(e);
    if (mergeTicket === null) throw e;
    return { status: "merge-required", mergeTicket };
  }
  await getClientRuntime().refreshUser();
  return { status: "linked" };
}

// The server answers 409 with `{ message, data: { reason, mergeTicket } }`.
function getMergeTicket(e: unknown): string | null {
  if (!(e instanceof WaspAuthClientError)) return null;
  const details = (e.data as { data?: Record<string, unknown> } | undefined)
    ?.data;
  return details?.reason === "merge-required" &&
    typeof details.mergeTicket === "string"
    ? details.mergeTicket
    : null;
}

// PUBLIC API
/** Adds a username and password to the signed-in user's account. */
export function linkUsername(data: {
  username: string;
  password: string;
}): Promise<LinkResult> {
  return linkThrough("/username/link", data);
}

// PUBLIC API
/**
 * Adds an email and password to the signed-in user's account. The address
 * must be verified through the emailed link before it can be used to log in.
 */
export function linkEmail(data: {
  email: string;
  password: string;
}): Promise<LinkResult> {
  return linkThrough("/email/link", data);
}

// PUBLIC API
/**
 * The second step of a merge, after the user agreed: the other account's
 * data and logins move into the signed-in one, and the other account is
 * deleted. Not reversible.
 */
export async function confirmMerge(mergeTicket: string): Promise<void> {
  await post(`${basePath()}/merge`, { mergeTicket });
  await getClientRuntime().refreshUser();
}

// PUBLIC API
/**
 * Disconnects one of `user.identities` from the signed-in user's account.
 * Rejects (409) when it is the account's only login method.
 */
export async function unlink(identity: LinkedIdentity): Promise<void> {
  await post(`${basePath()}/unlink`, {
    method: identity.providerName,
    providerUserId: identity.providerUserId,
  });
  await getClientRuntime().refreshUser();
}

// PUBLIC API
/**
 * Sends the browser to the OAuth provider to connect it to the signed-in
 * user's account. A navigation cannot carry a bearer credential, so the
 * client first fetches a short-lived signed link intent with its credential
 * and puts that in the URL.
 */
export async function startOAuthLink(
  provider: OAuthProviderName,
): Promise<void> {
  const { linkIntent } = await post<{ linkIntent: string }>(
    `${basePath()}/link-intent`,
    {},
  );
  window.location.href = `${basePath()}/${provider}/login?intent=link&linkIntent=${encodeURIComponent(linkIntent)}`;
}
