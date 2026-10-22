import { createAction } from "../operations/actions/core";
import { createQuery } from "../operations/queries/core";
import { makeUseActionFor, makeUseQueryFor, type ActionFor, type QueryFor, type UseActionFor, type UseQueryFor } from "@wasp.sh/lib-sdk-core/browser";
import type {
  RegisteredGetAllQuery,
} from "../../server/crud/taskVotes";

// PUBLIC API
export const taskVotes: TaskVoteCrud = createCrud();

// PUBLIC API
export interface TaskVoteCrud {
  getAll: TaskVoteGetAll;
};

export interface TaskVoteGetAll {
  query: QueryFor<RegisteredGetAllQuery>;
  useQuery: UseQueryFor<RegisteredGetAllQuery>;
};

function createCrud(): TaskVoteCrud {
  const crudGetAllQuery = createQuery<RegisteredGetAllQuery>(
    'crud/taskVotes/get-all',
    ["TaskVote"]
  )
  return {
    getAll: {
      query: crudGetAllQuery,
      useQuery: makeUseQueryFor(crudGetAllQuery)
    },
  }
}
