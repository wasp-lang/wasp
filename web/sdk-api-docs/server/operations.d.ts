/**
 * Server-side APIs for your
 * [Queries](https://wasp.sh/docs/features/data/operations/queries) and
 * [Actions](https://wasp.sh/docs/features/data/operations/actions): the types
 * for implementing them, and the functions for calling them from other server
 * code.
 *
 * Wasp generates two exports for each Query and Action you declare, both named
 * after it. For example, for a Query declared as `getFoo`:
 *
 * - The function `getFoo`, which calls the Query from server code. Its type is
 *   {@link AuthenticatedOperation} or {@link UnauthenticatedOperation},
 *   depending on whether the Query uses auth.
 * - The generic type `GetFoo<Input, Output>`, for typing the Query's
 *   implementation. It's an {@link AuthenticatedQueryDefinition} or an
 *   {@link UnauthenticatedQueryDefinition}, depending on whether the Query
 *   uses auth. For Actions, it's an {@link AuthenticatedActionDefinition} or
 *   an {@link UnauthenticatedActionDefinition}.
 *
 * An Operation uses auth if your app uses auth, unless you set `auth: false`
 * in its spec.
 *
 * To call Queries and Actions from the client, use `wasp/client/operations`
 * instead.
 *
 * @example
 * ```ts
 * // Use it on the server
 * import { getFoo } from "wasp/server/operations";
 * // Type its implementation
 * import { type GetFoo } from "wasp/server/operations";
 * ```
 *
 * @module
 */

import type { Payload } from "@wasp.sh/lib-sdk-core";
import type { AuthUser } from "../auth";

/**
 * The type of an authenticated Query's implementation.
 *
 * After you declare a Query, Wasp generates a generic type you can use when
 * defining its implementation. For the Query declared as `getSomething`, the
 * generated type is called `GetSomething`, and it's an
 * `AuthenticatedQueryDefinition` if the Query uses auth (otherwise it's an
 * {@link UnauthenticatedQueryDefinition}):
 *
 * ```ts
 * import { type GetSomething } from "wasp/server/operations";
 * ```
 *
 * The generated type fills in `Entities` with the Entities from the Query's
 * spec, and expects two (optional) type arguments:
 *
 * 1. `Input`: The type of the `args` object (the Query's input payload). The
 *    default value is `never`.
 * 2. `Output`: The type of the Query's return value (the Query's output
 *    payload). The default value is {@link Payload} (any serializable value).
 *
 * The defaults were chosen to make the type signature as permissive as
 * possible. If you don't want your Query to take/return anything, use `void`
 * as a type argument.
 *
 * Using the type is optional, but it's very helpful since it properly types
 * the Query's `context`: TypeScript will know which Entities are in
 * `context.entities`, and that `context` includes the `user`. Specifying
 * `Input` and `Output` gives you type support for the arguments and the
 * return value inside the implementation, and **full-stack type safety**: the
 * client-side Query will have the same types.
 *
 * The implementation is a Node.js function that takes two arguments (it can be
 * an `async` function if you need to use the `await` keyword). Since both
 * arguments are positional, you can name the parameters however you want, but
 * we'll stick with `args` and `context`:
 *
 * 1. `args`: An object containing the data **passed in when calling the
 *    Query** (e.g., filtering conditions).
 * 2. `context`: An additional context object **passed into the Query by
 *    Wasp**. It contains the logged-in `user`, and the Entities the Query uses
 *    in `entities`.
 *
 * @typeParam Entities The type of `context.entities`: an object with the
 * Prisma API for each Entity in the Query's spec (e.g., `{ Task: ... }` for
 * `entities: ["Task"]`). The generated type fills it in for you.
 * @typeParam Input The type of the Query's `args` (its input payload).
 * @typeParam Output The type of the Query's return value (its output payload).
 *
 * @example
 * The Query `getFoo`, declared like this:
 *
 * ```ts
 * import { app, query } from "@wasp.sh/spec";
 * import { getFoo } from "./src/queries" with { type: "ref" };
 *
 * export default app({
 *   // ...
 *   spec: [
 *     query(getFoo, { entities: ["Foo"] }),
 *   ],
 * });
 * ```
 *
 * Expects to find a named export `getFoo` from the file `src/queries.ts`. You
 * can use the generated type `GetFoo` and specify the Query's inputs and
 * outputs using its type arguments:
 *
 * ```ts
 * import { type GetFoo } from "wasp/server/operations";
 *
 * type Foo = // ...
 *
 * export const getFoo: GetFoo<{ id: number }, Foo> = (args, context) => {
 *   // implementation
 * };
 * ```
 *
 * In this case, the Query expects to receive an object with an `id` field of
 * type `number` (this is the type of `args`), and return a value of type `Foo`
 * (this must match the type of the Query's return value).
 *
 * @example
 * If you don't want to explicitly type the Query's return value, the
 * `satisfies` keyword tells TypeScript to infer it automatically:
 *
 * ```ts
 * const getFoo = (async (_args, context) => {
 *   const foos = await context.entities.Foo.findMany();
 *   return {
 *     foos,
 *     message: "Here are some foos!",
 *     queriedAt: new Date(),
 *   };
 * }) satisfies GetFoo;
 * ```
 *
 * TypeScript knows the correct type for `context`, and that the Query's
 * return type is `{ foos: Foo[], message: string, queriedAt: Date }`.
 *
 * @see [Implementing Queries](https://wasp.sh/docs/features/data/operations/queries#implementing-queries-in-node)
 * @see [Using Entities in Queries](https://wasp.sh/docs/features/data/operations/queries#using-entities-in-queries)
 * @see [Using the `context.user` object](https://wasp.sh/docs/features/auth/overview#using-the-contextuser-object)
 */
export type AuthenticatedQueryDefinition<
  Entities,
  Input extends Payload,
  Output extends Payload,
> = (
  args: Input,
  context: AuthenticatedOperationContext<Entities>,
) => Output | Promise<Output>;

/**
 * The type of a Query's implementation, when the Query doesn't use auth.
 *
 * It's the same as {@link AuthenticatedQueryDefinition}, except that the
 * `context` doesn't include the `user`. Wasp uses it for the generated Query
 * types (e.g., `GetSomething`) when your app doesn't use auth, or when you set
 * `auth: false` in the Query's spec.
 *
 * @typeParam Entities The type of `context.entities`: an object with the
 * Prisma API for each Entity in the Query's spec (e.g., `{ Task: ... }` for
 * `entities: ["Task"]`). The generated type fills it in for you.
 * @typeParam Input The type of the Query's `args` (its input payload). The
 * generated type defaults it to `never`.
 * @typeParam Output The type of the Query's return value (its output payload).
 * The generated type defaults it to {@link Payload}.
 *
 * @example
 * ```ts
 * import { type Task } from "wasp/entities";
 * import { type GetFilteredTasks } from "wasp/server/operations";
 *
 * export const getFilteredTasks: GetFilteredTasks<
 *   Pick<Task, "isDone">,
 *   Task[]
 * > = async (args, context) => {
 *   return context.entities.Task.findMany({
 *     where: { isDone: args.isDone },
 *   });
 * };
 * ```
 *
 * @see [Implementing Queries](https://wasp.sh/docs/features/data/operations/queries#implementing-queries-in-node)
 */
export type UnauthenticatedQueryDefinition<
  Entities,
  Input extends Payload,
  Output extends Payload,
> = (
  args: Input,
  context: OperationContext<Entities>,
) => Output | Promise<Output>;

/**
 * The type of an authenticated Action's implementation.
 *
 * After you declare an Action, Wasp generates a generic type you can use when
 * defining its implementation. For the Action declared as `createSomething`,
 * the generated type is called `CreateSomething`, and it's an
 * `AuthenticatedActionDefinition` if the Action uses auth (otherwise it's an
 * {@link UnauthenticatedActionDefinition}):
 *
 * ```ts
 * import { type CreateSomething } from "wasp/server/operations";
 * ```
 *
 * The generated type fills in `Entities` with the Entities from the Action's
 * spec, and expects two (optional) type arguments:
 *
 * 1. `Input`: The type of the `args` object (the Action's input payload). The
 *    default value is `never`.
 * 2. `Output`: The type of the Action's return value (the Action's output
 *    payload). The default value is {@link Payload} (any serializable value).
 *
 * The defaults were chosen to make the type signature as permissive as
 * possible. If you don't want your Action to take/return anything, use `void`
 * as a type argument.
 *
 * Using the type is optional, but it's very helpful since it properly types
 * the Action's `context`: TypeScript will know which Entities are in
 * `context.entities`, and that `context` includes the `user`. Specifying
 * `Input` and `Output` gives you type support for the arguments and the
 * return value inside the implementation, and **full-stack type safety**: the
 * client-side Action will have the same types.
 *
 * The implementation is a Node.js function that takes two arguments (it can be
 * an `async` function if you need to use the `await` keyword). Since both
 * arguments are positional, you can name the parameters however you want, but
 * we'll stick with `args` and `context`:
 *
 * 1. `args`: An object containing the data **passed in when calling the
 *    Action** (e.g., the data for a new task).
 * 2. `context`: An additional context object **passed into the Action by
 *    Wasp**. It contains the logged-in `user`, and the Entities the Action uses
 *    in `entities`.
 *
 * @typeParam Entities The type of `context.entities`: an object with the
 * Prisma API for each Entity in the Action's spec (e.g., `{ Task: ... }` for
 * `entities: ["Task"]`). The generated type fills it in for you.
 * @typeParam Input The type of the Action's `args` (its input payload).
 * @typeParam Output The type of the Action's return value (its output payload).
 *
 * @example
 * The Action `createFoo`, declared like this:
 *
 * ```ts
 * import { action, app } from "@wasp.sh/spec";
 * import { createFoo } from "./src/actions" with { type: "ref" };
 *
 * export default app({
 *   // ...
 *   spec: [
 *     action(createFoo, { entities: ["Foo"] }),
 *   ],
 * });
 * ```
 *
 * Expects to find a named export `createFoo` from the file `src/actions.ts`.
 * You can use the generated type `CreateFoo` and specify the Action's inputs
 * and outputs using its type arguments:
 *
 * ```ts
 * import { type CreateFoo } from "wasp/server/operations";
 *
 * type Foo = // ...
 *
 * export const createFoo: CreateFoo<{ bar: string }, Foo> = (args, context) => {
 *   // implementation
 * };
 * ```
 *
 * In this case, the Action expects to receive an object with a `bar` field of
 * type `string` (this is the type of `args`), and return a value of type `Foo`
 * (this must match the type of the Action's return value).
 *
 * @example
 * If you don't want to explicitly type the Action's return value, the
 * `satisfies` keyword tells TypeScript to infer it automatically:
 *
 * ```ts
 * const createFoo = (async (_args, context) => {
 *   const foo = await context.entities.Foo.create();
 *   return {
 *     newFoo: foo,
 *     message: "Here's your foo!",
 *     returnedAt: new Date(),
 *   };
 * }) satisfies CreateFoo;
 * ```
 *
 * TypeScript knows the correct type for `context`, and that the Action's
 * return type is `{ newFoo: Foo, message: string, returnedAt: Date }`.
 *
 * @see [Implementing Actions](https://wasp.sh/docs/features/data/operations/actions#implementing-actions-in-node)
 * @see [Using Entities in Actions](https://wasp.sh/docs/features/data/operations/actions#using-entities-in-actions)
 * @see [Using the `context.user` object](https://wasp.sh/docs/features/auth/overview#using-the-contextuser-object)
 */
export type AuthenticatedActionDefinition<
  Entities,
  Input extends Payload,
  Output extends Payload,
> = (
  args: Input,
  context: AuthenticatedOperationContext<Entities>,
) => Output | Promise<Output>;

/**
 * The type of an Action's implementation, when the Action doesn't use auth.
 *
 * It's the same as {@link AuthenticatedActionDefinition}, except that the
 * `context` doesn't include the `user`. Wasp uses it for the generated Action
 * types (e.g., `CreateSomething`) when your app doesn't use auth, or when you
 * set `auth: false` in the Action's spec.
 *
 * @typeParam Entities The type of `context.entities`: an object with the
 * Prisma API for each Entity in the Action's spec (e.g., `{ Task: ... }` for
 * `entities: ["Task"]`). The generated type fills it in for you.
 * @typeParam Input The type of the Action's `args` (its input payload). The
 * generated type defaults it to `never`.
 * @typeParam Output The type of the Action's return value (its output
 * payload). The generated type defaults it to {@link Payload}.
 *
 * @example
 * ```ts
 * import { type Task } from "wasp/entities";
 * import { type CreateTask } from "wasp/server/operations";
 *
 * export const createTask: CreateTask<Pick<Task, "description">, Task> = (
 *   args,
 *   context,
 * ) => {
 *   return context.entities.Task.create({
 *     data: { description: args.description },
 *   });
 * };
 * ```
 *
 * @see [Implementing Actions](https://wasp.sh/docs/features/data/operations/actions#implementing-actions-in-node)
 */
export type UnauthenticatedActionDefinition<
  Entities,
  Input extends Payload,
  Output extends Payload,
> = (
  args: Input,
  context: OperationContext<Entities>,
) => Output | Promise<Output>;

/**
 * The type of the functions for calling authenticated Queries and Actions on
 * the server.
 *
 * Wasp generates one such function for each Query and Action that uses auth,
 * named after it (e.g., `getFoo` for a Query declared as `getFoo`). Import it
 * from `wasp/server/operations`.
 *
 * Calling an Operation on the server is similar to calling it on the client.
 * The difference is that you must pass in a context object with the `user`
 * as the last argument. You don't have to pass the other parts of the
 * `context` object, like Entities: Wasp injects them automatically.
 *
 * The function's arguments depend on the Operation's `Input` type:
 *
 * - If it's `void`, the function only takes the context: `getFoo({ user })`.
 * - Otherwise, it takes the payload and the context:
 *   `getFoo(args, { user })`.
 *
 * It returns a `Promise` with the Operation's result.
 *
 * @typeParam Input The type of the Operation's `args` (its input payload).
 * @typeParam Output The type of the Operation's return value (its output
 * payload).
 *
 * @example
 * ```ts
 * import { getAllTasks, getFilteredTasks } from "wasp/server/operations";
 *
 * const user = // Get an AuthUser object, e.g., from context.user in an operation.
 *
 * // TypeScript automatically infers the return values and type-checks
 * // the payloads.
 * const allTasks = await getAllTasks({ user });
 * const doneTasks = await getFilteredTasks({ isDone: true }, { user });
 * ```
 *
 * @example
 * ```ts
 * import { createTask, markTaskAsDone } from "wasp/server/operations";
 *
 * const user = // Get an AuthUser object, e.g., from context.user
 *
 * const newTask = await createTask(
 *   { description: "Keep learning TypeScript" },
 *   { user },
 * );
 * await markTaskAsDone({ id: 1 }, { user });
 * ```
 *
 * @see [Using Queries on the server](https://wasp.sh/docs/features/data/operations/queries#using-queries-on-the-server)
 * @see [Using Actions on the server](https://wasp.sh/docs/features/data/operations/actions#using-actions-on-the-server)
 */
export type AuthenticatedOperation<Input, Output> =
  // `Input` is `any`
  0 extends 1 & Input
    ? (args: any, context: { user: AuthUser }) => Promise<Output>
    : [Input] extends [never]
      ? (args: unknown, context: { user: AuthUser }) => Promise<Output>
      : [Input] extends [void]
        ? (context: { user: AuthUser }) => Promise<Output>
        : (args: Input, context: { user: AuthUser }) => Promise<Output>;

/**
 * The type of the functions for calling Queries and Actions that don't use
 * auth on the server.
 *
 * Wasp generates one such function for each Query and Action that doesn't use
 * auth, named after it (e.g., `getFoo` for a Query declared as `getFoo`).
 * Import it from `wasp/server/operations`.
 *
 * The function takes the Operation's payload as its only argument (or no
 * arguments, if the Operation's `Input` type is `void`), and returns a
 * `Promise` with the Operation's result. You don't have to pass the `context`
 * object: Wasp injects the Entities automatically.
 *
 * @typeParam Input The type of the Operation's `args` (its input payload).
 * @typeParam Output The type of the Operation's return value (its output
 * payload).
 *
 * @example
 * ```ts
 * import { getAllTasks, getFilteredTasks } from "wasp/server/operations";
 *
 * const allTasks = await getAllTasks();
 * const doneTasks = await getFilteredTasks({ isDone: true });
 * ```
 *
 * @see [Using Queries on the server](https://wasp.sh/docs/features/data/operations/queries#using-queries-on-the-server)
 */
export type UnauthenticatedOperation<Input, Output> =
  // `Input` is `any`
  0 extends 1 & Input
    ? (args?: any) => Promise<Output>
    : [Input] extends [never]
      ? (args?: unknown) => Promise<Output>
      : [Input] extends [void]
        ? () => Promise<Output>
        : (args: Input) => Promise<Output>;

/**
 * The `context` Wasp passes to the implementation of a Query or Action that
 * doesn't use auth.
 */
type OperationContext<Entities> = {
  /**
   * The Prisma API for each Entity in the Operation's spec, keyed by the
   * Entity's name. For example, `context.entities.Task` exposes `prisma.task`
   * from [Prisma's CRUD API](https://www.prisma.io/docs/orm/prisma-client/queries/crud).
   *
   * @see [Using Entities in Queries](https://wasp.sh/docs/features/data/operations/queries#using-entities-in-queries)
   */
  entities: Entities;
};

/**
 * The `context` Wasp passes to the implementation of a Query or Action that
 * uses auth.
 */
type AuthenticatedOperationContext<Entities> = {
  /**
   * The Prisma API for each Entity in the Operation's spec, keyed by the
   * Entity's name. For example, `context.entities.Task` exposes `prisma.task`
   * from [Prisma's CRUD API](https://www.prisma.io/docs/orm/prisma-client/queries/crud).
   *
   * @see [Using Entities in Queries](https://wasp.sh/docs/features/data/operations/queries#using-entities-in-queries)
   */
  entities: Entities;
  /**
   * The logged-in user, or `undefined` if the request isn't authenticated.
   *
   * @see [Using the `context.user` object](https://wasp.sh/docs/features/auth/overview#using-the-contextuser-object)
   */
  user?: AuthUser;
};

export {};
