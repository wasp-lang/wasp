{{={= =}=}}
import { createAction, type ActionFor } from "../operations/actions/core.js";
import { createQuery, type QueryFor } from "../operations/queries/core.js";
import { makeUseActionFor, makeUseQueryFor, type UseActionFor, type UseQueryFor } from "./operationsHelpers.js";
import { 
  {=# operations.Get =}
  GetQueryResolved,
  {=/ operations.Get =}
  {=# operations.GetAll =}
  GetAllQueryResolved,
  {=/ operations.GetAll =}
  {=# operations.Create =}
  CreateActionResolved,
  {=/ operations.Create =}
  {=# operations.Update =}
  UpdateActionResolved,
  {=/ operations.Update =}
  {=# operations.Delete =}
  DeleteActionResolved,
  {=/ operations.Delete =}
} from 'wasp/server/crud/{= name =}'

type Crud = {
  get: {
    query: GetQuery,
    useQuery: UseQueryFor<GetQuery>
  },
  getAll: {
    query: GetAllQuery,
    useQuery: UseQueryFor<GetAllQuery>
  },
  create: {
    action: CreateAction,
    useAction: UseActionFor<CreateAction>,
  },
  update: {
    action: UpdateAction,
    useAction: UseActionFor<UpdateAction>,
  },
  delete: {
    action: DeleteAction,
    useAction: UseActionFor<DeleteAction>,
  }
};

type GetQuery = QueryFor<GetQueryResolved>;
type GetAllQuery = QueryFor<GetAllQueryResolved>;
type CreateAction = ActionFor<CreateActionResolved>;
type UpdateAction = ActionFor<UpdateActionResolved>;
type DeleteAction = ActionFor<DeleteActionResolved>;

// PUBLIC API
export const {= name =}: Crud = createCrud();

function createCrud(): Crud {
  {=# operations.Get =}
  const crudGetQuery = createQuery<GetQueryResolved>(
    '{= fullPath =}',
    {=& entitiesArray =}
  )
  {=/ operations.Get =}
  {=# operations.GetAll =}
  const crudGetAllQuery = createQuery<GetAllQueryResolved>(
    '{= fullPath =}',
    {=& entitiesArray =}
  )
  {=/ operations.GetAll =}
  {=# operations.Create =}
  const crudCreateAction = createAction<CreateActionResolved>(
    '{= fullPath =}',
    {=& entitiesArray =}
  )
  {=/ operations.Create =}
  {=# operations.Update =}
  const crudUpdateAction = createAction<UpdateActionResolved>(
    '{= fullPath =}',
    {=& entitiesArray =}
  )
  {=/ operations.Update =}
  {=# operations.Delete =}
  const crudDeleteAction = createAction<DeleteActionResolved>(
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
