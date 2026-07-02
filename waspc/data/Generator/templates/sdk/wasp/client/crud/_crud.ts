{{={= =}=}}
import { createAction, type ActionFor } from "../operations/actions/core.js";
import { createQuery, type QueryFor } from "../operations/queries/core.js";
import { makeUseActionFor, makeUseQueryFor, type UseActionFor, type UseQueryFor } from "./operationsHelpers.js";
import {
  {=# operations.Get =}
  RegisteredGetQuery,
  {=/ operations.Get =}
  {=# operations.GetAll =}
  RegisteredGetAllQuery,
  {=/ operations.GetAll =}
  {=# operations.Create =}
  RegisteredCreateAction,
  {=/ operations.Create =}
  {=# operations.Update =}
  RegisteredUpdateAction,
  {=/ operations.Update =}
  {=# operations.Delete =}
  RegisteredDeleteAction,
  {=/ operations.Delete =}
} from '../../server/crud/{= name =}'


// PUBLIC API
export const {= name =}: {= entityUpper =}Crud = createCrud();

// PUBLIC API
export type {= entityUpper =}Crud = {
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

function createCrud(): {= entityUpper =}Crud {
  {=# operations.Get =}
  const crudGetQuery = createQuery<RegisteredGetQuery>(
    '{= fullPath =}',
    {=& entitiesArray =}
  )
  {=/ operations.Get =}
  {=# operations.GetAll =}
  const crudGetAllQuery = createQuery<RegisteredGetAllQuery>(
    '{= fullPath =}',
    {=& entitiesArray =}
  )
  {=/ operations.GetAll =}
  {=# operations.Create =}
  const crudCreateAction = createAction<RegisteredCreateAction>(
    '{= fullPath =}',
    {=& entitiesArray =}
  )
  {=/ operations.Create =}
  {=# operations.Update =}
  const crudUpdateAction = createAction<RegisteredUpdateAction>(
    '{= fullPath =}',
    {=& entitiesArray =}
  )
  {=/ operations.Update =}
  {=# operations.Delete =}
  const crudDeleteAction = createAction<RegisteredDeleteAction>(
    '{= fullPath =}',
    {=& entitiesArray =}
  )
  {=/ operations.Delete =}
  return {
    {=# operations.Get =}
    get: {
      query: crudGetQuery,
      useQuery: makeUseQueryFor(crudGetQuery)
    },
    {=/ operations.Get =}
    {=# operations.GetAll =}
    getAll: {
      query: crudGetAllQuery,
      useQuery: makeUseQueryFor(crudGetAllQuery)
    },
    {=/ operations.GetAll =}
    {=# operations.Create =}
    create: {
      action: crudCreateAction,
      useAction: makeUseActionFor(crudCreateAction)
    },
    {=/ operations.Create =}
    {=# operations.Update =}
    update: {
      action: crudUpdateAction,
      useAction: makeUseActionFor(crudUpdateAction)
    },
    {=/ operations.Update =}
    {=# operations.Delete =}
    delete: {
      action: crudDeleteAction,
      useAction: makeUseActionFor(crudDeleteAction)
    },
    {=/ operations.Delete =}
  }
}

