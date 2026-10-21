import { createOneTimeCodeStore } from "@wasp.sh/lib-auth/node";
import { jwt } from "../jwt.js";

// PRIVATE API
export const tokenStore = createOneTimeCodeStore(jwt);
