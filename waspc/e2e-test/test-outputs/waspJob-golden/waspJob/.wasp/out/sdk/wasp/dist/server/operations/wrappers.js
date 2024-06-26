// PRIVATE API (used in SDK)
/**
 * Creates the server-side API for an unauthenticated operation.
 *
 * @template OperationDefinition The type of the unauthenticated operation's definition.
 * @param userOperation The unauthenticated operation's definition.
 * @param entities The unauthenticated operation's entity map .
 * @returns The server-side API for the provided unauthenticated operation.
 */
export function createUnauthenticatedOperation(userOperation, entities) {
    async function operation(payload) {
        return userOperation(payload, {
            entities,
        });
    }
    // This assertion is necessary because, when the Input is void, we want to present
    // the function as not accepting a payload (which isn't consistent with how
    // it's defined).
    return operation;
}
//# sourceMappingURL=wrappers.js.map