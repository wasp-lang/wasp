import { createQuery } from "./core";
// PUBLIC API
export const getMyTasks = createQuery("operations/get-my-tasks", ['Task']);
// PRIVATE API (used in SDK)
export { buildAndRegisterQuery } from "./core";
//# sourceMappingURL=index.js.map