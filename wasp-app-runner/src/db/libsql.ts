import type { SetupDbFn } from "./types.js";

declare global {
  namespace NodeJS {
    interface ProcessEnv {
      DATABASE_URL?: string;
      DATABASE_AUTH_TOKEN?: string;
    }
  }
}

export const setupLibsql: SetupDbFn = async ({ appName, pathToApp }) => {
  const databaseUrl = process.env.DATABASE_URL;
  const authToken = process.env.DATABASE_AUTH_TOKEN;

  if (!databaseUrl || !authToken) {
    throw new Error(
      "DATABASE_URL and DATABASE_AUTH_TOKEN environment variables must be set for Turso database"
    );
  }

  // Ensure the URL is in the correct format for Prisma
  const prismaUrl = databaseUrl.startsWith("libsql://") 
    ? databaseUrl 
    : `libsql://${databaseUrl}`;

  return {
    dbEnvVars: {
      DATABASE_URL: prismaUrl,
      DATABASE_AUTH_TOKEN: authToken,
    },
  };
}; 
