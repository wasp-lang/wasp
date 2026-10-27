/**
 * Client-side APIs for the Queries and Actions of your
 * [Automatic CRUD](https://wasp.sh/docs/features/data/crud) specs.
 *
 * Wasp generates one export for each `crud` spec you declare, named after it
 * (e.g., `Tasks` for `crud("Tasks", "Task", { ... })`). Each one is a
 * {@link Crud} object, with a field for each operation you enabled in the
 * spec.
 *
 * All CRUD operations are implemented with
 * [Queries and Actions](https://wasp.sh/docs/features/data/operations/overview)
 * under the hood, which means they come with all the features you'd expect
 * (e.g., automatic SuperJSON serialization, full-stack type safety when using
 * TypeScript).
 *
 * @example
 * ```tsx
 * import { Tasks } from "wasp/client/crud";
 *
 * export const SomePage = () => {
 *   const { data: tasks } = Tasks.getAll.useQuery();
 *   const { data: task } = Tasks.get.useQuery({ id: 1 });
 *   const createAction = Tasks.create.useAction();
 *   const updateAction = Tasks.update.useAction();
 *   const deleteAction = Tasks.delete.useAction();
 *   // ...
 * };
 * ```
 *
 * @module
 */

import type { UseQueryResult } from "@tanstack/react-query";
import type {
  Action,
  OptimisticUpdateDefinition,
  Query,
  useAction,
  useQuery,
} from "./operations";

/**
 * The type of the client-side object for a `crud` spec.
 *
 * Wasp generates one such export for each `crud` spec you declare, named after
 * it (e.g., `Tasks` for `crud("Tasks", "Task", { ... })`). Import it from
 * `wasp/client/crud`.
 *
 * It only has the fields for the operations you enabled in the spec. In
 * TypeScript, their input and output types come from the operations' default
 * implementations, or from your overrides (see
 * [`wasp/server/crud`](https://wasp.sh/docs/api/wasp/server/crud)).
 *
 * This type itself isn't exported from `wasp/client/crud`.
 *
 * @example
 * ```tsx
 * import { Tasks } from "wasp/client/crud";
 * import { useState } from "react";
 *
 * export const MainPage = () => {
 *   // Thanks to full-stack type safety, all payload types are inferred
 *   // automatically.
 *   const { data: tasks, isLoading, error } = Tasks.getAll.useQuery();
 *   const createTask = Tasks.create.useAction();
 *   const [taskDescription, setTaskDescription] = useState("");
 *
 *   function handleCreateTask() {
 *     createTask({ description: taskDescription, isDone: false });
 *     setTaskDescription("");
 *   }
 *
 *   if (isLoading) return <div>Loading...</div>;
 *   if (error) return <div>Error: {error.message}</div>;
 *   return (
 *     <div>
 *       <input
 *         value={taskDescription}
 *         onChange={(e) => setTaskDescription(e.target.value)}
 *       />
 *       <button onClick={handleCreateTask}>Create task</button>
 *       <ul>
 *         {tasks.map((task) => (
 *           <li key={task.id}>{task.description}</li>
 *         ))}
 *       </ul>
 *     </div>
 *   );
 * };
 * ```
 *
 * @see [Using the generated CRUD operations on the client](https://wasp.sh/docs/features/data/crud#using-the-generated-crud-operations-on-the-client)
 */
export interface Crud {
  /**
   * Gets a single Entity. Present if you enabled the `get` operation.
   *
   * By default, it takes the Entity's unique fields (e.g., `{ id: 1 }`), and
   * returns the matching Entity, or `null` if there isn't one.
   *
   * @example
   * ```ts
   * const { data: task } = Tasks.get.useQuery({ id: 1 });
   * ```
   */
  get?: CrudQuery<any, any>;
  /**
   * Gets all the Entities. Present if you enabled the `getAll` operation.
   *
   * By default, it takes no arguments, and returns an array with all the
   * Entities.
   *
   * @example
   * ```ts
   * const { data: tasks } = Tasks.getAll.useQuery();
   * ```
   */
  getAll?: CrudQuery<any, any>;
  /**
   * Creates an Entity. Present if you enabled the `create` operation.
   *
   * By default, it takes the new Entity's data, and returns the created
   * Entity.
   *
   * @example
   * ```ts
   * const createTask = Tasks.create.useAction();
   * await createTask({ description: "Buy some eggs", isDone: false });
   * ```
   */
  create?: CrudAction<any, any>;
  /**
   * Updates an Entity. Present if you enabled the `update` operation.
   *
   * By default, it takes the Entity's unique fields (e.g., `id`) and the
   * fields to update, and returns the updated Entity.
   *
   * @example
   * ```ts
   * const updateTask = Tasks.update.useAction();
   * await updateTask({ id: 1, isDone: true });
   * ```
   */
  update?: CrudAction<any, any>;
  /**
   * Deletes an Entity. Present if you enabled the `delete` operation.
   *
   * By default, it takes the Entity's unique fields (e.g., `{ id: 1 }`), and
   * returns the deleted Entity.
   *
   * @example
   * ```ts
   * const deleteTask = Tasks.delete.useAction();
   * await deleteTask({ id: 1 });
   * ```
   */
  delete?: CrudAction<any, any>;
}

/**
 * A CRUD Query (`get` or `getAll`) on the client.
 *
 * @typeParam Input The type of the Query's input payload.
 * @typeParam Output The type of the Query's return value.
 */
interface CrudQuery<Input, Output> {
  /**
   * The Query itself. You can call it directly, or pass it to
   * {@link useQuery}.
   */
  query: Query<Input, Output>;
  /**
   * Calls the Query with {@link useQuery}.
   *
   * @param queryFnArgs The arguments object (payload) to pass to the Query.
   * @param options A _react-query_ `options` object for this particular
   * Query.
   */
  useQuery: (
    queryFnArgs?: Input,
    options?: any,
  ) => UseQueryResult<Output, Error>;
}

/**
 * A CRUD Action (`create`, `update`, or `delete`) on the client.
 *
 * @typeParam Input The type of the Action's input payload.
 * @typeParam Output The type of the Action's return value.
 */
interface CrudAction<Input, Output> {
  /**
   * The Action itself. You can call it directly.
   */
  action: Action<Input, Output>;
  /**
   * Enhances the Action with {@link useAction} (e.g., to add optimistic
   * updates).
   *
   * @param actionOptions The options for {@link useAction}.
   * @returns A function with the same API as the Action.
   */
  useAction: (actionOptions?: {
    optimisticUpdates: OptimisticUpdateDefinition<Input, any>[];
  }) => Action<Input, Output>;
}

export {};
