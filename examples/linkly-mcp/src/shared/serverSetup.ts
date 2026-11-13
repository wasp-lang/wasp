import { env, type ServerSetupFn } from "wasp/server";
import { configureUrls } from "./urls";

// Wasp's env config is the source of truth for public URLs at runtime.
export const serverSetup: ServerSetupFn = async () => {
  configureUrls({
    serverUrl: env.WASP_SERVER_URL,
    frontendUrl: env.WASP_WEB_CLIENT_URL,
  });
};
