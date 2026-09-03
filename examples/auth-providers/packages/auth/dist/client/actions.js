import { post } from "./http.js";
import { getClientOptions, getClientRuntime } from "./runtime.js";
/** The server path prefix the routes live under. */
function basePath() {
    return getClientOptions().routesBasePath ?? "/auth/wasp";
}
async function initSession(sessionId) {
    // The provider-bound sink: records 'wasp' as the minting provider and
    // refreshes cached queries so `useAuth` sees the new user.
    await getClientRuntime().setSession(sessionId);
}
// PUBLIC API
export async function login(data) {
    const path = "email" in data
        ? `${basePath()}/email/login`
        : `${basePath()}/username/login`;
    const { sessionId } = await post(path, data);
    await initSession(sessionId);
}
// PUBLIC API
export async function signup(data) {
    const path = "email" in data
        ? `${basePath()}/email/signup`
        : `${basePath()}/username/signup`;
    const result = await post(path, data);
    return { success: result.success ?? true };
}
// PUBLIC API
export async function requestPasswordReset(data) {
    const { success } = await post(`${basePath()}/email/request-password-reset`, data);
    return { success };
}
// PUBLIC API
export async function resetPassword(data) {
    const { success } = await post(`${basePath()}/email/reset-password`, data);
    return { success };
}
// PUBLIC API
export async function verifyEmail(data) {
    return post(`${basePath()}/email/verify-email`, data);
}
// PRIVATE API
export async function exchangeOAuthCodeForSession(code) {
    const { sessionId } = await post(`${basePath()}/exchange-code`, { code });
    await initSession(sessionId);
}
export function signInUrl(provider) {
    return `${getClientRuntime().apiUrl}${basePath()}/${provider}/login`;
}
export function isMethodEnabled(name) {
    return getClientOptions().methods[name] !== undefined;
}
