import { WaspAuthClientError, post } from "./http.js";
import { getClientRuntime, getClientSpec } from "./runtime.js";
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
    await getClientRuntime().setCredential(typeof body.credential === "string" ? body.credential : null, { persistent: body.persistent !== false });
}
// PUBLIC API
/**
 * `persistent: false` is a login without "remember me": the credential lasts
 * for the browser session only.
 */
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
    return getClientSpec().methods[name] !== undefined;
}
async function linkThrough(path, data) {
    try {
        await post(`${basePath()}${path}`, data);
    }
    catch (e) {
        const mergeTicket = getMergeTicket(e);
        if (mergeTicket === null)
            throw e;
        return { status: "merge-required", mergeTicket };
    }
    await getClientRuntime().refreshUser();
    return { status: "linked" };
}
// The server answers 409 with `{ message, data: { reason, mergeTicket } }`.
function getMergeTicket(e) {
    if (!(e instanceof WaspAuthClientError))
        return null;
    const details = e.data
        ?.data;
    return details?.reason === "merge-required" &&
        typeof details.mergeTicket === "string"
        ? details.mergeTicket
        : null;
}
// PUBLIC API
/** Adds a username and password to the signed-in user's account. */
export function linkUsername(data) {
    return linkThrough("/username/link", data);
}
// PUBLIC API
/**
 * Adds an email and password to the signed-in user's account. The address
 * must be verified through the emailed link before it can be used to log in.
 */
export function linkEmail(data) {
    return linkThrough("/email/link", data);
}
// PUBLIC API
/**
 * The second step of a merge, after the user agreed: the other account's
 * data and logins move into the signed-in one, and the other account is
 * deleted. Not reversible.
 */
export async function confirmMerge(mergeTicket) {
    await post(`${basePath()}/merge`, { mergeTicket });
    await getClientRuntime().refreshUser();
}
// PUBLIC API
/**
 * Disconnects one of `user.identities` from the signed-in user's account.
 * Rejects (409) when it is the account's only login method.
 */
export async function unlink(identity) {
    await post(`${basePath()}/unlink`, {
        method: identity.providerName.substring(identity.providerName.indexOf(":") + 1),
        subjectId: identity.providerUserId,
    });
    await getClientRuntime().refreshUser();
}
// PUBLIC API
/**
 * Sends the browser to the OAuth provider to connect it to the signed-in
 * user's account. A navigation cannot carry a bearer credential, so the
 * credential is first traded for a one-time code. There is none when the
 * credential is a cookie, which the navigation carries by itself.
 */
export async function startOAuthLink(provider) {
    const { oneTimeCode } = await post(`${basePath()}/link-intent`, {});
    const oneTimeCodeParam = oneTimeCode === null
        ? ""
        : `&oneTimeCode=${encodeURIComponent(oneTimeCode)}`;
    window.location.href = `${basePath()}/${provider}/login?intent=link${oneTimeCodeParam}`;
}
