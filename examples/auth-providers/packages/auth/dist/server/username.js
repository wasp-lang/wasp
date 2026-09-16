import { hashPassword, verifyPassword } from "@wasp.sh/lib-auth/node";
import { getBody, json, sendAuthResponse } from "./http.js";
import { namespaceFor } from "./namespaces.js";
import { createInvalidCredentialsError, rethrowPossibleAuthError, validateAndGetUserFields, } from "./utils.js";
import { ensurePasswordIsPresent, ensureValidPassword, ensureValidUsername, normalizeUsername, } from "./validation.js";
/** The username & password method: `/auth/username/{login,signup}`. */
export function usernameRoutes({ runtime, extensions }) {
    const identities = () => runtime.identityNamespaces(namespaceFor(runtime, "username"));
    return [
        {
            method: "POST",
            path: "/username/login",
            handler: async (req, res) => {
                const fields = getBody(req);
                ensureValidUsername(fields);
                ensurePasswordIsPresent(fields);
                const username = normalizeUsername(fields.username);
                const identity = await identities().find(username);
                if (!identity) {
                    throw createInvalidCredentialsError();
                }
                try {
                    const secrets = await identities().getSecrets(username);
                    if (secrets === null || typeof secrets.hashedPassword !== "string") {
                        throw createInvalidCredentialsError();
                    }
                    await verifyPassword(secrets.hashedPassword, fields.password);
                }
                catch {
                    throw createInvalidCredentialsError();
                }
                // The sign-in goes through the credentials facet any handler gets;
                // the app's login hooks fire inside it, and the credentials scheme
                // decides what the client receives.
                const { response } = await runtime.credentials.signIn({ namespace: namespaceFor(runtime, "username"), subjectId: username }, { req });
                sendAuthResponse(res, response);
            },
        },
        {
            method: "POST",
            path: "/username/signup",
            handler: async (req, res) => {
                const fields = getBody(req);
                ensureValidUsername(fields);
                ensurePasswordIsPresent(fields);
                ensureValidPassword(fields);
                try {
                    // The facet's `create` is the signup choke point: the app's
                    // onBeforeSignup veto, then the lazy userSignupFields getters, then
                    // the atomic write, then onAfterSignup.
                    await identities().create(normalizeUsername(fields.username), {
                        secrets: {
                            hashedPassword: await hashPassword(fields.password),
                        },
                    }, (() => validateAndGetUserFields(fields, extensions.userSignupFields?.username)), { req });
                }
                catch (e) {
                    rethrowPossibleAuthError(e);
                }
                json(res, 200, { success: true });
            },
        },
    ];
}
