// PRIVATE API (used in SDK)
/**
 * Creates the server-side API for an unauthenticated operation.
 *
 * The operation definition is accepted through a getter instead of directly to
 * prevent "use before initialization" errors. When a user's operation definition
 * imports another operation's server-side API (i.e., imports from `"wasp/server/operations"`),
 * that module and ours form a cycle. Depending on which side of the cycle the
 * bundler enters first, it can emit our {@link createUnauthenticatedOperation}
 * wrapper calls above the user's operation definitions:
 * ```ts
 * function badCreateOperation(fn: Function) {
 *   return () => fn();
 * }
 * const badServerOperation = badCreateOperation(someUserOperation);
 *       ^! ReferenceError: Cannot access 'someUserOperation' before initialization
 * const someUserOperation = () => 1;
 * ```
 *
 * The getter defers the read until the operation is called, by which point every
 * module is initialized:
 * ```ts
 * function goodCreateOperation(fn: () => Function) {
 *   return () => fn()();
 * }
 * const goodServerOperation = goodCreateOperation(() => someUserOperation);
 * const someUserOperation = () => 1;
 * ```
 * @template OperationDefinition The type of the unauthenticated operation's definition.
 * @param getUserOperation Returns the unauthenticated operation's definition.
 * @param entities The unauthenticated operation's entity map.
 * @returns The server-side API for the provided unauthenticated operation.
 */
export function createUnauthenticatedOperation(getUserOperation, entities) {
    async function operation(payload) {
        return getUserOperation()(payload, {
            entities,
        });
    }
    // This assertion is necessary because, when the Input is void, we want to present
    // the function as not accepting a payload (which isn't consistent with how
    // it's defined).
    return operation;
}
// PRIVATE API (used in SDK)
/**
 * Creates the server-side API for an authenticated operation.
 * For an explanation of why we accept an operation definition getter instead
 * of the value directly, check {@link createUnauthenticatedOperation}.
 *
 * @template OperationDefinition The type of the authenticated operation's definition.
 * @param getUserOperation Returns the authenticated operation's definition.
 * @param entities The authenticated operation's entity map.
 * @returns The server-side API for the provided authenticated operation.
 */
export function createAuthenticatedOperation(getUserOperation, entities) {
    async function operation(...args) {
        // To understand this function and its limitations, read
        // https://github.com/wasp-lang/wasp/issues/2050
        if (args.length < 1) {
            // No arguments sent -> no user and no payload specified -> there's no way this was called correctly.
            throw new Error(`
        You called the operation without arguments, which is a mistake.
        Check your definition and read the docs to understand what you need to send:
        https://wasp.sh/docs/data-model/operations/overview
        `);
        }
        else if (includesPayload(args)) {
            // Two arguments sent -> the first argument is the payload, the second is the context.
            const [payload, context] = args;
            return getUserOperation()(payload, {
                ...context,
                entities,
            });
        }
        else {
            // One argument sent -> the first and only argument is the user.
            const [context] = args;
            return getUserOperation()(undefined, {
                ...context,
                entities,
            });
        }
    }
    return operation;
}
/**
 * Returns a boolean carrying compiler type information about whether the
 * provided arguments array (of an authenticated operation) includes a payload.
 *
 * To understand why "two arguments" means "includes payload", read
 * https://github.com/wasp-lang/wasp/issues/2050
 *
 * @template Input The type of the payload the operation expects.
 * @param args The arguments array for an authenticated operation.
 * @returns true if the arguments array includes a payload, false otherwise.
 */
function includesPayload(args) {
    return args.length === 2;
}
//# sourceMappingURL=wrappers.js.map