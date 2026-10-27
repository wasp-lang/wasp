/**
 * Types for overriding the Queries and Actions of your
 * [Automatic CRUD](https://wasp.sh/docs/features/data/crud) specs.
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
 * Wasp generates one type-only namespace export for each `crud` spec you
 * declare, named after it (e.g., `Tasks` for `crud("Tasks", "Task", { ... })`).
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
 * In other words, annotating an override with one of these types tells
 * TypeScript about the type of the operation's `context` object, while the
 * two type arguments allow you to specify the operation's inputs and outputs.
 * `Input` defaults to `never`, and `Output` defaults to any serializable
 * value.
 *
 * Wasp also generates a `wasp/server/crud/<crud name>` import path for each
 * `crud` spec, which exports the same namespace.
 *
 * @example
 * If you have a CRUD named `Tasks`, you would import the types like this:
 *
 * ```ts
 * import { type Tasks } from "wasp/server/crud";
 *
 * // Each of the types is a generic type, so you can use it like this:
 * export const getAllOverride: Tasks.GetAllQuery<Input, Output> = async (
 *   args,
 *   context,
 * ) => {
 *   // ...
 * };
 * ```
 *
 * @example
 * A custom `create` operation that connects the new task to the user creating
 * it:
 *
 * ```ts
 * import { type Tasks } from "wasp/server/crud";
 * import { type Task } from "wasp/entities";
 * import { HttpError } from "wasp/server";
 *
 * type CreateTaskInput = { description: string; isDone: boolean };
 *
 * export const createTask: Tasks.CreateAction<CreateTaskInput, Task> = async (
 *   args,
 *   context,
 * ) => {
 *   if (!context.user) {
 *     throw new HttpError(401, "User not authenticated.");
 *   }
 *
 *   const { description, isDone } = args;
 *   const { Task } = context.entities;
 *
 *   return await Task.create({
 *     data: {
 *       description,
 *       isDone,
 *       // Connect the task to the user that is creating it
 *       user: {
 *         connect: {
 *           id: context.user.id,
 *         },
 *       },
 *     },
 *   });
 * };
 * ```
 *
 * @see [Our custom `create` operation](https://wasp.sh/docs/features/data/crud#our-custom-create-operation)
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
