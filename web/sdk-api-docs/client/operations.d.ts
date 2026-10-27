/**
 * Client-side APIs for calling your Queries and Actions.
 *
 * @module
 */

import type { UseQueryResult } from "@tanstack/react-query";
import type { Action, Query } from "@wasp.sh/lib-sdk-core";
import type { ActionOptions } from "@wasp.sh/lib-sdk-core/browser";

/**
 * Calls a Query from a React component and keeps its result up to date.
 *
 * Wasp invalidates the Query's cache automatically when you call an Action
 * that uses the same Entities.
 *
 * @param query The Query to call, imported from `wasp/client/operations`.
 * @param queryArgs The arguments to pass to the Query.
 * @param options Options passed through to React Query's `useQuery`.
 *
 * @example
 * ```tsx
 * import { getTasks, useQuery } from "wasp/client/operations";
 *
 * export function TaskList() {
 *   const { data: tasks, isLoading, error } = useQuery(getTasks);
 *   // ...
 * }
 * ```
 *
 * @see [Using Queries](https://wasp.sh/docs/features/data/operations/queries#using-queries-on-the-client)
 */
export declare function useQuery<Input, Output>(
  query: Query<Input, Output>,
  queryArgs?: Input,
  options?: any,
): UseQueryResult<Output, Error>;

/**
 * Adds extra behavior to an Action, like optimistic updates.
 *
 * You don't need this hook to call an Action: you can call it directly.
 *
 * @param action The Action to enhance, imported from `wasp/client/operations`.
 * @param actionOptions The behavior to add.
 * @returns An Action with the same signature as `action`.
 *
 * @see [The `useAction` hook and optimistic updates](https://wasp.sh/docs/features/data/operations/actions#the-useaction-hook-and-optimistic-updates)
 */
export declare function useAction<Input = unknown, Output = unknown>(
  action: Action<Input, Output>,
  actionOptions?: ActionOptions<Input>,
): Action<Input, Output>;

export type { Action, Query } from "@wasp.sh/lib-sdk-core";
