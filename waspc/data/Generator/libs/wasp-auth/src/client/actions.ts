import { post } from "./http.js";
import { getClientOptions, getClientRuntime } from "./runtime.js";
import type { OAuthProviderName } from "./types.js";

async function initSession(sessionId: string): Promise<void> {
  // The provider-bound sink: records 'wasp' as the minting provider and
  // refreshes cached queries so `useAuth` sees the new user.
  await getClientRuntime().setSession(sessionId);
}

// PUBLIC API
export async function login(
  data:
    | { username: string; password: string }
    | { email: string; password: string },
): Promise<void> {
  const path = "email" in data ? "/auth/email/login" : "/auth/username/login";
  const { sessionId } = await post<{ sessionId: string }>(path, data);
  await initSession(sessionId);
}

// PUBLIC API
export async function signup(
  data: (
    | { username: string; password: string }
    | { email: string; password: string }
  ) &
    Record<string, unknown>,
): Promise<{ success: boolean }> {
  const path = "email" in data ? "/auth/email/signup" : "/auth/username/signup";
  const result = await post<{ success?: boolean }>(path, data);
  return { success: result.success ?? true };
}

// PUBLIC API
export async function requestPasswordReset(data: {
  email: string;
}): Promise<{ success: boolean }> {
  const { success } = await post<{ success: boolean }>(
    "/auth/email/request-password-reset",
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
    "/auth/email/reset-password",
    data,
  );
  return { success };
}

// PUBLIC API
export async function verifyEmail(data: {
  token: string;
}): Promise<{ success: boolean; reason?: string }> {
  return post<{ success: boolean; reason?: string }>(
    "/auth/email/verify-email",
    data,
  );
}

// PRIVATE API
export async function exchangeOAuthCodeForSession(code: string): Promise<void> {
  const { sessionId } = await post<{ sessionId: string }>(
    "/auth/exchange-code",
    { code },
  );
  await initSession(sessionId);
}

export function signInUrl(provider: OAuthProviderName): string {
  return `${getClientRuntime().apiUrl}/auth/${provider}/login`;
}

export function isMethodEnabled(
  name: keyof ReturnType<typeof getClientOptions>["methods"],
): boolean {
  return getClientOptions().methods[name] !== undefined;
}
