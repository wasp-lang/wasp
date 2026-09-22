import type {
  SignInAs,
  SignOutHere,
  SignOutEverywhereRoute,
  WhoAmI,
} from "wasp/server/api";
import { prisma } from "wasp/server";
import {
  authenticate,
  getIdentityStore,
  signIn,
  signOut,
  signOutEverywhere,
} from "wasp/server/auth";

/**
 * The imperative auth API from ordinary `api()` routes: the same things the
 * scheme's own routes and the auth middleware do, called by app code.
 * `signInAs` is an impersonation endpoint, which is why it is not something
 * to ship, only something to show.
 */

export const signInAs: SignInAs = async (req, res) => {
  const { email } = req.body as { email?: unknown };
  const identity = await getIdentityStore("password", "default").find(
    String(email),
  );
  const auth =
    identity &&
    (await prisma.auth.findUnique({
      where: { id: identity.authId },
      include: { user: true },
    }));
  const user = auth?.user;
  if (!user) {
    res.status(404).json({ message: "No such user." });
    return;
  }
  // Writes the credential to `res`: `{ credential }` for this bearer scheme.
  await signIn(user, res, { req, properties: { ttl: "1h" } });
};

export const whoAmI: WhoAmI = async (req, res) => {
  // Public route that still knows who is asking.
  const user = await authenticate(req);
  res.json({ userId: user?.id ?? null });
};

export const signOutHere: SignOutHere = async (req, res) => {
  await signOut(req, res);
};

export const signOutEverywhereRoute: SignOutEverywhereRoute = async (
  _req,
  res,
  context,
) => {
  await signOutEverywhere(context.user!);
  res.json({ success: true });
};
