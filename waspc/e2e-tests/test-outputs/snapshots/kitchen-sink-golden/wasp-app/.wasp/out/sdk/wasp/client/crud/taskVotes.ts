import { createAction, type ActionFor } from "../operations/actions/core";
import { createQuery, type QueryFor } from "../operations/queries/core";
import { makeUseActionFor, makeUseQueryFor, type UseActionFor, type UseQueryFor } from "./operationsHelpers";
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
    ['TaskVote']
  )
  return {
    getAll: {
      query: crudGetAllQuery,
      useQuery: makeUseQueryFor(crudGetAllQuery)
    },
  }
}

