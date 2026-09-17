import { post } from "./http.js";
import { getClientOptions, getClientRuntime } from "./runtime.js";
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
  name: keyof ReturnType<typeof getClientOptions>["methods"],
): boolean {
  return getClientOptions().methods[name] !== undefined;
}
