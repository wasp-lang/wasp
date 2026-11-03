/**
 * Types for your app's
 * [Entities](https://wasp.sh/docs/features/data/entities), which you can use
 * on both the server and the client.
 *
 * Wasp generates one type export per Entity, named after it (e.g., `Task` for
 * a `Task` model in your `schema.prisma` file). Each one is the type Prisma
 * generates for that model, so it describes the Entity's fields, and is
 * assignable to {@link Entity}. When you change an Entity in your
 * `schema.prisma` file, the type changes with it.
 *
 * If your app uses auth, this module also exports the `Auth` and
 * `AuthIdentity` types for Wasp's
 * [auth Entities](https://wasp.sh/docs/features/auth/entities). They aren't
 * part of {@link Entity} or {@link EntityName}.
 *
 * @example
 * ```tsx
 * import { type Task } from "wasp/entities";
 *
 * export function ExamplePage() {
 *   const task: Task = {
 *     id: "some-uuid-1234",
 *     description: "Some random task",
 *     isDone: false,
 *   };
 *   return <div>{task.description}</div>;
 * }
 * ```
 *
 * @see [Entities](https://wasp.sh/docs/features/data/entities)
 *
 * @module
 */

/**
 * Any of your app's Entity types.
 *
 * In your app, this is the union of the types of all the Entities you defined
 * in your `schema.prisma` file (e.g., `User | Task`).
 */
export type Entity = object;

/**
 * The name of any of your app's Entities.
 *
 * In your app, this is the union of the names of all the Entities you defined
 * in your `schema.prisma` file (e.g., `"User" | "Task"`).
 */
export type EntityName = string;
