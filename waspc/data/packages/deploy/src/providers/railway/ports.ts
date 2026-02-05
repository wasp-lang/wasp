import { Port } from "./brandedTypes";

// Ports we decided to use for the server and client apps.
// These are arbitrary values that are used internally by
// Railway containers to run the apps.
export const serverAppPort = 8080 as Port;
// Port for static client (Railway static file server)
export const clientAppPortStatic = 8080 as Port;
// Port for SSR client (Node.js server)
export const clientAppPortSsr = 3000 as Port;

// Default client port (for backward compatibility)
export const clientAppPort = clientAppPortStatic;
