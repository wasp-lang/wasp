import { hash } from "@node-rs/argon2";
import { HttpError } from "wasp/server";
import { type Signup } from "wasp/server/api";
import { getAuthContractErrorCode } from "wasp/server/auth/handler/types";
import { normalizeEmail, runtime } from "./handler";

/**
 * The scheme's signup endpoint, an ordinary Wasp `api()` route.
 *
 * Everything here is the same machinery Wasp's own email auth uses: the
 * identities facet creates User + Auth + AuthIdentity in one atomic write
 * (firing the app's signup hooks around it), the password hash goes into the
 * `secrets` channel (hashed HERE, explicitly -- storage never hashes), and
 * the asserted email into `claims`.
 *
 * NOTE: deliberately minimal -- no email verification, no anti-enumeration
 * fake work (compare Wasp's own email auth, which does both). This app shows
 * the storage and credential mechanics, not a production signup flow.
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
    await runtime().identities.create(
      normalizedEmail,
      {
        claims: { email: normalizedEmail },
        secrets: { hashedPassword: await hash(password) },
      },
      undefined,
      { req },
    );
  } catch (e: unknown) {
    if (getAuthContractErrorCode(e) === "wasp-auth/duplicate-identity") {
      throw new HttpError(422, "An account with this email already exists.");
    }
    throw e;
  }

  res.json({ success: true });
};
