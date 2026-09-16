import { verify } from "@node-rs/argon2";
import { HttpError } from "wasp/server";
import { type Login } from "wasp/server/api";
import { normalizeEmail, runtime } from "./handler";

/**
 * The scheme's login endpoint, an ordinary Wasp `api()` route.
 *
 * Verifies the password against the `secrets` channel, then signs the subject
 * in through the scheme's credentials facet: the app's login hooks fire, the
 * private issuer mints a bearer token, and the answer (`{ credential }`) is
 * written to the response as the issuer shaped it. A wrong password and an
 * unknown email are the same 401, so the endpoint reveals no accounts.
 */
export const login: Login = async (req, res) => {
  const { email, password } = (req.body ?? {}) as {
    email?: string;
    password?: string;
  };
  if (typeof email !== "string" || typeof password !== "string") {
    throw new HttpError(401, "Invalid credentials");
  }

  const normalizedEmail = normalizeEmail(email);
  const secrets = await runtime().identities.getSecrets(normalizedEmail);
  if (secrets === null || typeof secrets.hashedPassword !== "string") {
    throw new HttpError(401, "Invalid credentials");
  }
  const passwordMatches = await verify(secrets.hashedPassword, password).catch(
    () => false,
  );
  if (!passwordMatches) {
    throw new HttpError(401, "Invalid credentials");
  }

  const { response } = await runtime().credentials.signIn(
    { subjectId: normalizedEmail },
    { req },
  );
  res.status(response.status);
  for (const [name, value] of Object.entries(response.headers ?? {})) {
    res.setHeader(name, value);
  }
  res.json(response.body ?? {});
};
