import { getEmail } from "@wasp.sh/auth/user";
import { type AuthUser } from "wasp/auth";

const oauthMethodLabels: Record<string, string> = {
  "wasp:google": "Google",
  "wasp:github": "GitHub",
  "wasp:discord": "Discord",
  "wasp:slack": "Slack",
  "wasp:microsoft": "Microsoft",
  "wasp:keycloak": "Keycloak",
};

export function getName(user?: AuthUser) {
  if (!user) {
    return null;
  }

  // We use multiple auth methods, so we need to check which one is available.
  const email = getEmail(user);
  if (email !== null) {
    return email;
  }

  const oauthIdentity = user.identities.find(
    (identity) => identity.providerName in oauthMethodLabels,
  );
  if (oauthIdentity) {
    const label = oauthMethodLabels[oauthIdentity.providerName];
    return `${label} user ${oauthIdentity.providerUserId}`;
  }

  // If we don't know how to get the name, return null.
  return null;
}
