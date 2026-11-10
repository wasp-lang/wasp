/**
 * Types for overriding the Queries and Actions of your
 * [Automatic CRUD](https://wasp.sh/docs/features/data/crud) specs.
 *
 * Wasp generates one type-only namespace export for each `crud` spec you
 * declare, named after it (e.g. `Tasks` for `crud("Tasks", "Task", { ... })`).
 * It has a generic type for each operation you enabled in the spec, which
 * you can use to type its override:
 *
 * - `<crud name>.GetAllQuery<Input, Output>`
 * - `<crud name>.GetQuery<Input, Output>`
 * - `<crud name>.CreateAction<Input, Output>`
 * - `<crud name>.UpdateAction<Input, Output>`
 * - `<crud name>.DeleteAction<Input, Output>`
 *
 * They work exactly like the types Wasp generates for Queries and Actions:
 * the `GetAllQuery` and `GetQuery` types are
 * {@link AuthenticatedQueryDefinition}s, and the `CreateAction`,
 * `UpdateAction`, and `DeleteAction` types are
 * {@link AuthenticatedActionDefinition}s, with the CRUD's Entity in
 * `context.entities`. If your app doesn't use auth, they're
 * {@link UnauthenticatedQueryDefinition}s and
 * {@link UnauthenticatedActionDefinition}s instead, and `context` doesn't
 * include the `user`.
 *
 * The two type arguments specify the operation's inputs and outputs. `Input`
 * defaults to `never`, and `Output` defaults to any serializable value.
 *
 * Like with Queries and Actions, you can define the implementation of a CRUD
 * operation (an override) in a JavaScript/TypeScript file. The overrides are
 * functions that take the following arguments:
 *
 * - `args`: The arguments of the operation, i.e., the data sent from the
 *   client.
 * - `context`: Contains the `user` making the request, and the `entities`
 *   object with the Entity that's being operated on.
 *
 * Wasp only checks that the request is authenticated in its default
 * implementations, so an override runs for anonymous requests too, and
 * `isPublic` has no effect on it. If your override requires a logged-in user,
 * check `context.user` yourself.
 *
 * For a complete example, see
 * [our custom `create` operation](https://wasp.sh/docs/features/data/crud#our-custom-create-operation).
 *
 * @example
 * If you have a CRUD named `Tasks`, you can type a `getAll` override that only
 * returns the logged-in user's tasks like this:
 *
 * ```ts
 * import { type Task } from "wasp/entities";
 * import { HttpError } from "wasp/server";
 * import { type Tasks } from "wasp/server/crud";
 *
 * export const getAllOverride: Tasks.GetAllQuery<void, Task[]> = async (
 *   _args,
 *   context,
 * ) => {
 *   if (!context.user) {
 *     throw new HttpError(401, "User not authenticated.");
 *   }
 *
 *   return context.entities.Task.findMany({
 *     where: { user: { id: context.user.id } },
 *   });
 * };
 * ```
 *
 * @module
 */

import type {
  AuthenticatedActionDefinition,
  AuthenticatedQueryDefinition,
  UnauthenticatedActionDefinition,
  UnauthenticatedQueryDefinition,
} from "./operations";

export {};
