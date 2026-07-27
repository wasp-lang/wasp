import { createAction, type ActionFor } from "../operations/actions/core";
import { createQuery, type QueryFor } from "../operations/queries/core";
import { makeUseActionFor, makeUseQueryFor, type UseActionFor, type UseQueryFor } from "./operationsHelpers";
import {
  RegisteredGetQuery,
  RegisteredGetAllQuery,
  RegisteredCreateAction,
  RegisteredUpdateAction,
  RegisteredDeleteAction,
} from '../../server/crud/tasks'


// PUBLIC API
export const tasks: TaskCrud = createCrud();

// PUBLIC API
export type TaskCrud = {
  get: {
    query: QueryFor<RegisteredGetQuery>,
    useQuery: UseQueryFor<RegisteredGetQuery>
  },
  getAll: {
    query: QueryFor<RegisteredGetAllQuery>,
    useQuery: UseQueryFor<RegisteredGetAllQuery>
  },
  create: {
    action: ActionFor<RegisteredCreateAction>,
    useAction: UseActionFor<RegisteredCreateAction>,
  },
  update: {
    action: ActionFor<RegisteredUpdateAction>,
    useAction: UseActionFor<RegisteredUpdateAction>,
  },
  delete: {
    action: ActionFor<RegisteredDeleteAction>,
    useAction: UseActionFor<RegisteredDeleteAction>,
  }
};

function createCrud(): TaskCrud {
  const crudGetQuery = createQuery<RegisteredGetQuery>(
    'crud/tasks/get',
    ['Task']
  )
  const crudGetAllQuery = createQuery<RegisteredGetAllQuery>(
    'crud/tasks/get-all',
    ['Task']
  )
  const crudCreateAction = createAction<RegisteredCreateAction>(
    'crud/tasks/create',
    ['Task']
  )
  const crudUpdateAction = createAction<RegisteredUpdateAction>(
    'crud/tasks/update',
    ['Task']
  )
  const crudDeleteAction = createAction<RegisteredDeleteAction>(
    'crud/tasks/delete',
    ['Task']
  )
  return {
    get: {
      query: crudGetQuery,
      useQuery: makeUseQueryFor(crudGetQuery)
    },
    getAll: {
      query: crudGetAllQuery,
      useQuery: makeUseQueryFor(crudGetAllQuery)
    },
    create: {
      action: crudCreateAction,
      useAction: makeUseActionFor(crudCreateAction)
    },
    update: {
      action: crudUpdateAction,
      useAction: makeUseActionFor(crudUpdateAction)
    },
    delete: {
      action: crudDeleteAction,
      useAction: makeUseActionFor(crudDeleteAction)
    },
  }
}

