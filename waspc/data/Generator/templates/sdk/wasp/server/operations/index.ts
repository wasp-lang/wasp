{{={= =}=}}
// PUBLIC API
export * from './queries/types.js'
// PUBLIC API
export * from './actions/types.js'
{=# queries =}

export { {= operationName =} } from './queries/index.js'
{=/ queries =}
{=# actions =}

export { {= operationName =} } from './actions/index.js'
{=/ actions =}

/**
 * We must re-export {@link AuthUser} from "wasp/server/operations".
 * 
 * User code is compiled with declaration emit enabled. This is because
 * the server and the client TypeScript project reference the user's
 * TypeScript project. As such, the user project must have `composite: true`
 * flag which forces declaration emit.
 * 
 * When declaration emit is enabled, every exported binding without an
 * explicit type annotation gets its inferred type serialized into a
 * `.d.ts` file.
 * A common example is users defining operations while typing them with
 * the `satisfies` keyword.` 
 * 
 * If that inferred type structurally contains a type symbol from a
 * dependency package, `tsc` must synthesize a portable reference to
 * that symbol.
 * E.g., an authenticated operations context contains a type symbol for
 * {@link AuthUser}. So we must be able to create a portable reference to
 * {@link AuthUser}.
 * 
 * Creating a portable reference is possible if either:
 * 
 * 1. The symbol is imported locally.
 *     E.g., the same module that defines the operation, imports
 *    {@link AuthUser}: `import { type AuthUser } from "wasp/auth"`.
 * 
 * 2. The symbol is re-exported through an already loaded module.
 *    E.g., some other module has: `import { X } from "auth/wasp"`.
 *    Because of that, `tsc` can reach {@link AuthUser} through "auth/wasp".
 * 
 * 3. The file where the symbol is declared is reachable through the
 *    packages `exports` map.
 *    E.g., Wasp SDK does not export `"server/operations/user"` module,
 *    where {@link AuthUser} is declared, so this condition always fails.
 * 
 * So we must depend on either the first or the second condition
 * being true for types to work properly.
 * 
 * The first condition will most likely never happen naturally.
 * We can't depend on it being satisfied.
 * 
 * The second condition is almost always satisfied. But, there can be
 * situations where this is not true. Example being our very own
 * `ask-the-documents` project. It has zero `"wasp/auth"` imports,
 * but defines operations via the `satisfies` keyword. This results
 * in an error, because the {@link AuthUser} symbol is not portable.
 * 
 * To avoid this problem, `"wasp/server/operations"` re-export {@link AuthUser}.
 * That way, `tsc` can always create a portable reference to it.
 */
export { type AuthUser } from '../auth/user.js'
