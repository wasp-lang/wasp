import type { SetupDbFn } from "./types.js";

declare global {
  namespace NodeJS {
    interface ProcessEnv {
      TURSO_DATABASE_URL?: string;
      TURSO_AUTH_TOKEN?: string;
    }
  }
}

export const setupLibsql: SetupDbFn = async ({ appName, pathToApp }) => {
  const databaseUrl = process.env.DATABASE_URL;
  const authToken = process.env.AUTH_TOKEN;

  if (!databaseUrl || !authToken) {
    throw new Error(
      "DATABASE_URL and AUTH_TOKEN environment variables must be set for Turso database"
    );
  }

  // Ensure the URL is in the correct format for Prisma
  const prismaUrl = databaseUrl.startsWith("libsql://") 
    ? databaseUrl 
    : `libsql://${databaseUrl}`;

  return {
    dbEnvVars: {
      DATABASE_URL: prismaUrl,
      AUTH_TOKEN: authToken,
    },
  };
}; 
