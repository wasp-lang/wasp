import { HttpError } from "wasp/server";
import type { OnBeforeLinkHook } from "wasp/server/auth";

/**
 * The app's veto over account linking. It fires inside Wasp, where the
 * identity is attached, so no login method can skip it.
 */
export const onBeforeLink: OnBeforeLinkHook = async ({ providerId }) => {
  if (providerId.providerUserId.startsWith("reserved-")) {
    throw new HttpError(403, "That username is reserved.");
  }
};
