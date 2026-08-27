import { hash } from "@node-rs/argon2";
import { HttpError } from "wasp/server";
import { type Signup } from "wasp/server/api";
import { identities, normalizeEmail } from "./provider";

/**
 * The provider's signup endpoint, an ordinary Wasp `api()` route.
 *
 * Everything here is the same machinery Wasp's own email auth uses: the
 * identity store creates User + Auth + AuthIdentity in one atomic write, the
 * password hash goes into the `secrets` channel (hashed HERE, explicitly --
 * storage never hashes), and the asserted email into `claims`.
 *
 * NOTE: deliberately minimal -- no email verification, no anti-enumeration
 * fake work (compare Wasp's own email auth, which does both). This app shows
 * the storage and session mechanics, not a production signup flow.
 */
export const signup: Signup = async (req, res) => {
  const { email, password } = (req.body ?? {}) as {
    email?: string;
    password?: string;
  };
  if (typeof email !== "string" || !email.includes("@")) {
    throw new HttpError(400, "A valid email is required.");
  }
  if (typeof password !== "string" || password.length < 8) {
    throw new HttpError(400, "Password must be at least 8 characters long.");
  }

  const normalizedEmail = normalizeEmail(email);
  try {
    await identities.createIdentity(normalizedEmail, {
      claims: { email: normalizedEmail },
      secrets: { hashedPassword: await hash(password) },
    });
  } catch (e: unknown) {
    // Prisma's unique-constraint violation -- the identity already exists.
    if (
      typeof e === "object" &&
      e !== null &&
      "code" in e &&
      e.code === "P2002"
    ) {
      throw new HttpError(422, "An account with this email already exists.");
    }
    throw e;
  }

  res.json({ success: true });
};
