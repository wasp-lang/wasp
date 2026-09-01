import type { OnAfterLoginHook, OnBeforeSignupHook } from "wasp/server/auth";

/**
 * App-level lifecycle hooks (`auth.hooks` in main.wasp.ts).
 *
 * These fire at Wasp-owned choke points -- identity provisioning and session
 * minting -- so they cover the `@wasp.sh/auth` PACKAGE's flows too: the
 * adapter never calls them, Wasp does, which is what makes the veto below
 * impossible for a provider to skip.
 */

export const onBeforeSignup: OnBeforeSignupHook = async ({ providerId }) => {
  // Veto: throwing here rejects the signup before any user fields are
  // computed, whichever provider (or method namespace) is signing up.
  if (providerId.providerUserId.includes("blocked")) {
    throw new Error("This name is not allowed.");
  }
  console.log(
    `[hooks] onBeforeSignup: ${providerId.providerName}/${providerId.providerUserId}`,
  );
};

export const onAfterLogin: OnAfterLoginHook = async ({ providerId, user }) => {
  console.log(
    `[hooks] onAfterLogin: ${providerId.providerName}/${providerId.providerUserId} -> user ${user.id}`,
  );
};
