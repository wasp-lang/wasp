import { createSessions } from "@wasp.sh/lib-auth/node";
import {
  createAuthUserData,
  type AppAuthEntityTypes,
  type AuthUserData,
} from "../../auth/user.js";
import { prisma } from "../index.js";

// PRIVATE API
export const sessions = createSessions<AppAuthEntityTypes, AuthUserData>({
  prisma: {
    session: prisma.session,
    auth: prisma.auth,
    user: prisma.user,
  },
  authFieldOnUserEntityName: "auth",
  identitiesFieldOnAuthEntityName: "identities",
  createAuthUserData,
});

// PRIVATE API
export const {
  createSession,
  getSessionAndUserFromBearerToken,
  getSessionAndUserFromSessionId,
  invalidateSession,
  invalidateAllSessionsForAuthId,
} = sessions;
