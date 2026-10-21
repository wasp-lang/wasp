import { createLogout } from "@wasp.sh/lib-auth/browser";
import { api } from "../api/index.js";

// PUBLIC API
export default createLogout({ api });
