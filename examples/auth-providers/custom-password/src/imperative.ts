import type {
  ListSessions,
  SignInAs,
  SignOutCredentialRoute,
  SignOutEverywhereRoute,
  SignOutHere,
  WhoAmI,
} from "wasp/server/api";
import { prisma } from "wasp/server";
import {
  authenticate,
  getIdentityStore,
  signIn,
  signOut,
  signOutCredential,
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
  const identity = {
    handlerName: "password",
    providerName: "default",
    providerUserId: String(email),
  };
  const known = await getIdentityStore("password", "default").find(
    identity.providerUserId,
  );
  if (known === null) {
    res.status(404).json({ message: "No such user." });
    return;
  }
  // Names the identity exactly; Wasp resolves its scheme and its account.
  // Writes the credential to `res`: `{ credential }` for this bearer scheme.
  await signIn(identity, res, { properties: { ttl: "1h" } });
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

// The rows behind a "your active sessions" page. This scheme keeps its
// credentials in the `Session` table, so they can be listed and ended one by
// one; a signed-token scheme would have nothing to list.
export const listSessions: ListSessions = async (_req, res, context) => {
  const sessions = await prisma.session.findMany({
    where: { auth: { userId: context.user!.id } },
    select: { id: true, issuedAt: true, loginScheme: true },
    orderBy: { issuedAt: "asc" },
  });
  res.json({ sessions });
};

export const signOutCredentialRoute: SignOutCredentialRoute = async (
  req,
  res,
) => {
  const { credentialId } = req.body as { credentialId: string };
  await signOutCredential(credentialId);
  res.json({ success: true });
};
