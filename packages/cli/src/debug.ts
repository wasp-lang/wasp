import { debuglog } from "node:util";

// If you want to turn on debug logging, set the following environment variable:
// `NODE_DEBUG=wasp-bin-wrapper`
export const debug = debuglog("wasp-bin-wrapper");
