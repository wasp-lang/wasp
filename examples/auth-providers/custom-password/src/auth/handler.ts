import { hash, verify } from "@node-rs/argon2";
import {
  getAuthContractErrorCode,
  type ServerAuthHandlerFactory,
} from "wasp/server/auth/handler/types";

/**
 * Email+password auth, hand-rolled in-app -- the proof that a hand-written
 * scheme has the same powers a handler package has, because it IS the same
 * thing: a `ServerAuthHandlerFactory`, the function a package exports as
 * `createServerAuthHandler`. Pasting this file into a package needs no edits.
 *
 * - The runtime arrives as an argument: the identities facet for storage, and
 *   the credentials facet, because the manifest declares `credentials: {}`
 *   (Wasp runs a private bearer issuer for this scheme).
 * - It brings its own routes. Wasp mounts `routeHandler` at `/auth/password`,
 *   next to every other scheme's routes.
 * - Hashing is this app's explicit job (argon2, brought by this app -- Wasp
 *   ships no crypto to handlers).
 *
 * An app that would rather write ordinary Wasp `api()` routes can: keep the
 * factory for `handler`, stash `runtime` in a module variable here, and read
 * it from those routes. That is plain userland; Wasp needs no API for it.
 */
export const createPasswordAuthHandler: ServerAuthHandlerFactory<
  unknown,
  never,
  true
> = (runtime) => ({
  // The routes below verify logins. Afterwards a request carries the token
  // the issuer minted, and this handler recognizes it by forwarding to that
  // issuer -- the way ASP.NET's remote schemes forward to their sign-in scheme.
  handler: {
    authenticate: (request) => runtime.credentials.authenticate(request),
    signOut: (request) => runtime.credentials.signOut(request),
  },

  routeHandler: async (req, res) => {
    const send = (status: number, body: unknown) => {
      res.statusCode = status;
      res.setHeader("Content-Type", "application/json");
      res.end(JSON.stringify(body));
    };
    const { email, password } = ((req as { body?: unknown }).body ?? {}) as {
      email?: unknown;
      password?: unknown;
    };

    if (req.method === "POST" && req.url === "/signup") {
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
        await runtime.identities.default.create(
          normalizedEmail,
          {
            claims: { email: normalizedEmail },
            secrets: { hashedPassword: await hash(password) },
          },
          undefined,
          { req },
        );
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

    if (req.method === "POST" && req.url === "/login") {
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
      // client receives (here, `{ credential }`).
      const { response } = await runtime.credentials.signIn(
        { subjectId: normalizedEmail },
        { req },
      );
      for (const [name, value] of Object.entries(response.headers ?? {})) {
        res.setHeader(name, value);
      }
      return send(response.status, response.body ?? {});
    }

    send(404, { message: "Not found." });
  },
});

/**
 * The identity key. The store normalizes nothing for a custom scheme, so
 * casing discipline is the scheme's job -- signup and login must agree.
 */
function normalizeEmail(email: string): string {
  return email.trim().toLowerCase();
}
