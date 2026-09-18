import { hashPassword, verifyPassword } from "@wasp.sh/lib-auth/node";
import { getBody, getSignInProperties, json, sendAuthResponse, } from "./http.js";
import { offerMergeOrRethrow, requireCurrentAuthId } from "./linking.js";
import { createInvalidCredentialsError, rethrowPossibleAuthError, validateAndGetUserFields, } from "./utils.js";
import { ensurePasswordIsPresent, ensureValidPassword, ensureValidUsername, normalizeUsername, } from "./validation.js";
/** The username & password method: `/auth/username/{login,signup}`. */
export function usernameRoutes(ctx) {
    const { runtime, config } = ctx;
    const identities = () => runtime.identities.username;
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
                const { response } = await runtime.credentials.signIn({ namespace: "username", subjectId: username }, { req, properties: getSignInProperties(fields) });
                sendAuthResponse(res, response);
            },
        },
        {
            // Account linking: a username and password for the signed-in user.
            method: "POST",
            path: "/username/link",
            handler: async (req, res) => {
                const authId = await requireCurrentAuthId(ctx, req);
                const fields = getBody(req);
                ensureValidUsername(fields);
                ensurePasswordIsPresent(fields);
                ensureValidPassword(fields);
                const username = normalizeUsername(fields.username);
                try {
                    // `link`, not `create`: no new user, no userSignupFields; the
                    // app's link hooks fire inside.
                    await identities().link(username, {
                        secrets: {
                            hashedPassword: await hashPassword(fields.password),
                        },
                    }, { authId, req });
                }
                catch (e) {
                    // The username is taken. Knowing its password is the proof that
                    // the other account is the caller's own, and so may be merged.
                    await offerMergeOrRethrow(ctx, e, {
                        intoAuthId: authId,
                        findFromAuthId: async () => (await identities().find(username))?.authId ?? null,
                        proveControl: async () => {
                            const secrets = await identities().getSecrets(username);
                            if (typeof secrets?.hashedPassword !== "string")
                                return false;
                            return verifyPassword(secrets.hashedPassword, fields.password).then(() => true, () => false);
                        },
                    });
                }
                json(res, 200, { success: true });
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
                    }, (() => validateAndGetUserFields(fields, config.methods.usernameAndPassword?.userSignupFields)), { req });
                }
                catch (e) {
                    rethrowPossibleAuthError(e);
                }
                json(res, 200, { success: true });
            },
        },
    ];
}
