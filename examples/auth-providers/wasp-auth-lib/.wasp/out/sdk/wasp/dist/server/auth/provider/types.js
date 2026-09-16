// PRIVATE API
/**
 * The contract between Wasp and an authentication provider.
 *
 * The contract itself lives in the `@wasp.sh/auth-contract` package, so that
 * adapter packages can implement it as a normal npm dependency rather than
 * against code Wasp generates into someone's project. This module re-exports it
 * for Wasp's own internals and for adapters written inside the app.
 */
export { canManageSessions, canRevokeSessions, getAuthContractErrorCode, } from '@wasp.sh/auth-contract';
//# sourceMappingURL=types.js.map