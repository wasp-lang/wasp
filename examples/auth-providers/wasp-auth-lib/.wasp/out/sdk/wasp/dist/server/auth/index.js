export { defineUserSignupFields, } from '../../auth/providers/types.js';
// The identity store: the one channel for reading and writing auth
// identities. Wasp's own auth flows use the exact same facet a user-made
// provider gets -- no privileged access.
export { getIdentityStore, } from './identityStore.js';
export { createInvalidCredentialsError } from './utils.js';
//# sourceMappingURL=index.js.map