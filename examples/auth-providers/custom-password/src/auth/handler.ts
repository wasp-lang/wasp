import { hash, verify } from "@node-rs/argon2";
import {
  getAuthContractErrorCode,
  type ServerAuthAdapter,
} from "wasp/server/auth/handler/types";

/**
 * Email+password auth, hand-rolled in-app -- the proof that a hand-written
 * scheme has the same powers a handler package has, because it IS the same
 * thing: a `ServerAuthAdapter`, the function a package exports as
 * `createServerAuthHandler`. Pasting this file into a package needs no edits.
 *
 * - The runtime arrives as an argument: the identities facet for storage, and
 *   the credentials issuer, because the manifest declares `credentials: {}`
 *   (Wasp runs a private bearer issuer for this scheme).
 * - It brings its own routes. Wasp mounts `routeHandler` at `/auth/password`,
 *   next to every other scheme's routes.
 * - Hashing is this app's explicit job (argon2, brought by this app -- Wasp
 *   ships no crypto to handlers).
 *
 * An app that would rather write ordinary Wasp `api()` routes can: keep the
 * adapter for `handler`, stash `runtime` in a module variable here, and read
 * it from those routes. That is plain userland; Wasp needs no API for it.
 */
export const createPasswordAuthHandler: ServerAuthAdapter = (runtime) => ({
  // This handler is its routes. They verify logins and hand them to Wasp's
  // issuer (the manifest declares `credentials`); the credential a request
  // carries afterwards is Wasp's own, and Wasp recognises it itself, so
  // there is no AuthHandler to return.
  // Standard `Request` in, `Response` out. Wasp hands over the raw body, so
  // the route parses it itself.
  routeHandler: async (request) => {
    const send = (status: number, body: unknown) =>
      Response.json(body, { status });
    const url = new URL(request.url);
    const path = url.pathname.slice(runtime.mountPath.length);
    const { email, password } = (
      request.method === "POST" ? await request.json().catch(() => ({})) : {}
    ) as { email?: unknown; password?: unknown };

    if (request.method === "POST" && path === "/signup") {
      if (typeof email !== "string" || !email.includes("@")) {
        return send(400, { message: "A valid email is required." });
      }
      if (typeof password !== "string" || password.length < 8) {
        return send(400, {
          message: "Password must be at least 8 characters long.",
        });
      }
      const normalizedEmail = normalizeEmail(email);
      try {
        // One atomic write of User + Auth + AuthIdentity, with the app's
        // signup hooks fired around it. The hash goes into `secrets`, the
        // column the Prisma client omits by default.
        await runtime.identities.default.create(normalizedEmail, {
          identity: {
            claims: { email: normalizedEmail },
            secrets: { hashedPassword: await hash(password) },
          },
        });
      } catch (e) {
        if (getAuthContractErrorCode(e) === "wasp-auth/duplicate-identity") {
          return send(422, {
            message: "An account with this email already exists.",
          });
        }
        throw e;
      }
      return send(200, { success: true });
    }

    if (request.method === "POST" && path === "/login") {
      // A wrong password and an unknown email are the same 401, so the
      // endpoint reveals no accounts.
      if (typeof email !== "string" || typeof password !== "string") {
        return send(401, { message: "Invalid credentials" });
      }
      const normalizedEmail = normalizeEmail(email);
      const secrets =
        await runtime.identities.default.getSecrets(normalizedEmail);
      const passwordMatches =
        typeof secrets?.hashedPassword === "string" &&
        (await verify(secrets.hashedPassword, password).catch(() => false));
      if (!passwordMatches) {
        return send(401, { message: "Invalid credentials" });
      }
      // The app's login hooks fire inside; the issuer decides what the
      // client receives (here, `{ credential }`), as a standard Response.
      const { response } = await runtime.credentialsIssuer.signIn({
        providerUserId: normalizedEmail,
      });
      return response;
    }

    // A download is a browser NAVIGATION, and a navigation cannot carry the
    // bearer credential. The client first trades its credential for a
    // one-time code here (a normal request, so the header is attached)...
    if (request.method === "POST" && path === "/one-time-code") {
      try {
        const oneTimeCode =
          await runtime.credentialsIssuer.createOneTimeCode(request);
        return send(200, { oneTimeCode });
      } catch (e) {
        if (getAuthContractErrorCode(e) === "wasp-auth/unauthenticated") {
          return send(401, { message: "Invalid credentials" });
        }
        throw e;
      }
    }

    // ...and the navigation carries the code. It works once, for a minute.
    if (request.method === "GET" && path === "/export") {
      const account = await runtime.credentialsIssuer.redeemOneTimeCode(
        url.searchParams.get("oneTimeCode") ?? "",
      );
      if (account === null) {
        return send(401, { message: "Invalid credentials" });
      }
      return Response.json(
        { authId: account.authId },
        {
          headers: {
            "Content-Disposition": 'attachment; filename="account.json"',
          },
        },
      );
    }

    return send(404, { message: "Not found." });
  },
});

/**
 * The identity key. The store normalizes nothing for a custom scheme, so
 * casing discipline is the scheme's job -- signup and login must agree.
 */
function normalizeEmail(email: string): string {
  return email.trim().toLowerCase();
}
