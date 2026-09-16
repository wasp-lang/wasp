import { post } from "./http.js";
import { getClientOptions, getClientRuntime } from "./runtime.js";
/** The server path prefix the routes live under. */
function basePath() {
    return getClientRuntime().mountUrl;
}
/**
 * Adopts what the server answered a login with: a bearer credential in the
 * body, which the generated client stores and attaches to every request; or
 * nothing, when the credential travels as a cookie the browser already holds.
 * Either way the cached queries are refreshed, so `useAuth()` sees the login.
 */
async function adoptSignIn(body) {
    await getClientRuntime().setCredential(typeof body.credential === "string" ? body.credential : null);
}
// PUBLIC API
export async function login(data) {
    const path = "email" in data
        ? `${basePath()}/email/login`
        : `${basePath()}/username/login`;
    await adoptSignIn(await post(path, data));
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
    await adoptSignIn(await post(`${basePath()}/exchange-code`, {
        code,
    }));
}
export function signInUrl(provider) {
    return `${basePath()}/${provider}/login`;
}
export function isMethodEnabled(name) {
    return getClientOptions().methods[name] !== undefined;
}
