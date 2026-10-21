{{={= =}=}}
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
    session: prisma.{= sessionEntityLower =},
    auth: prisma.{= authEntityLower =},
    user: prisma.{= userEntityLower =},
  },
  authFieldOnUserEntityName: "{= authFieldOnUserEntityName =}",
  identitiesFieldOnAuthEntityName: "{= identitiesFieldOnAuthEntityName =}",
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
