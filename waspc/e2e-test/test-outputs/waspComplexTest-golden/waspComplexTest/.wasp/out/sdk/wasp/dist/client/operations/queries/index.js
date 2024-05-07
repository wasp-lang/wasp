import { createQuery } from './core';
// PUBLIC API
export const mySpecialQuery = createQuery('operations/my-special-query', ['User']);
// PRIVATE API (used in SDK)
export { buildAndRegisterQuery } from './core';
//# sourceMappingURL=index.js.map