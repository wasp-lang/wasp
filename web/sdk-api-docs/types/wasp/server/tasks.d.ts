/**
 * Types for implementing your app's [Tasks](https://wasp.sh/docs/features/tasks)
 * (administrative scripts you run from the terminal).
 *
 * Wasp generates one type per `task` you declare in the Wasp Spec, named after
 * its implementation function in PascalCase (e.g. `CreateAdmin` for
 * `createAdmin`). It's a {@link TaskDefinition} with the Task's Entities.
 *
 * To make sure Wasp generates the types, add your `task`s to the Wasp Spec
 * first, and keep `wasp start` running.
 *
 * @module
 */

/**
 * The type of a Task's Node.js implementation.
 *
 * The generated type (e.g. `CreateAdmin`) fills in `Entities` with the
 * Entities from the Task's spec, so it takes no type arguments.
 *
 * Since Wasp runs Tasks on the server, the implementation's import path must
 * lead to a Node.js file.
 *
 * @typeParam Entities The type of
 *   {@linkcode TaskContext.entities | context.entities}.
 *
 * @param args The command-line arguments after the Task's name, as strings.
 *   For `wasp task run createAdmin admin@example.com --dry-run`, they're
 *   `["admin@example.com", "--dry-run"]`. Use Node.js's
 *   [`parseArgs`](https://nodejs.org/api/util.html#utilparseargsconfig) to
 *   parse them.
 * @param context An additional context object Wasp injects into the Task.
 *
 * @returns Nothing. If the function throws (or its promise rejects), the
 *   command exits with a non-zero exit code.
 *
 * @example
 * With `entities: ["User"]` in the Task's spec:
 *
 * ```ts
 * import { parseArgs } from "node:util";
 * import type { CreateAdmin } from "wasp/server/tasks";
 *
 * export const createAdmin: CreateAdmin = async (args, context) => {
 *   const { positionals, values } = parseArgs({
 *     args,
 *     allowPositionals: true,
 *     options: { "dry-run": { type: "boolean" } },
 *   });
 *   const [email] = positionals;
 *
 *   if (values["dry-run"]) {
 *     console.log(`Would make ${email} an admin.`);
 *     return;
 *   }
 *
 *   await context.entities.User.update({
 *     where: { email },
 *     data: { isAdmin: true },
 *   });
 * };
 * ```
 *
 * @see [Implementing a Task](https://wasp.sh/docs/features/tasks#implementing-a-task)
 *
 * @placeholder
 */
export type TaskDefinition<Entities> = (
  args: string[],
  context: TaskContext<Entities>,
) => void | Promise<void>;

/**
 * The `context` Wasp passes to a Task's implementation.
 */
type TaskContext<Entities> = {
  /**
   * The Prisma API for each Entity in the Task's `entities` field, keyed by
   * the Entity's name (e.g. `context.entities.Task` exposes `prisma.task`
   * from
   * [Prisma's CRUD API](https://www.prisma.io/docs/orm/prisma-client/queries/crud)).
   */
  entities: Entities;
};
