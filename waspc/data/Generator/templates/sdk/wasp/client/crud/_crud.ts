{{={= =}=}}
import { createAction, type ActionFor } from "../operations/actions/core";
import { createQuery, type QueryFor } from "../operations/queries/core";
import { makeUseActionFor, makeUseQueryFor, type UseActionFor, type UseQueryFor } from "./operationsHelpers";
import type {
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
} from "../../server/crud/{= name =}";

// PUBLIC API
export const {= name =}: {= entityUpper =}Crud = createCrud();

// PUBLIC API
export interface {= entityUpper =}Crud {
  {=# operations.Get =}
  get: {= entityUpper =}Get;
  {=/ operations.Get =}
  {=# operations.GetAll =}
  getAll: {= entityUpper =}GetAll;
  {=/ operations.GetAll =}
  {=# operations.Create =}
  create: {= entityUpper =}Create;
  {=/ operations.Create =}
  {=# operations.Update =}
  update: {= entityUpper =}Update;
  {=/ operations.Update =}
  {=# operations.Delete =}
  delete: {= entityUpper =}Delete;
  {=/ operations.Delete =}
};
{=# operations.Get =}

export interface {= entityUpper =}Get {
  query: QueryFor<RegisteredGetQuery>;
  useQuery: UseQueryFor<RegisteredGetQuery>;
}
{=/ operations.Get =}
{=# operations.GetAll =}

export interface {= entityUpper =}GetAll {
  query: QueryFor<RegisteredGetAllQuery>;
  useQuery: UseQueryFor<RegisteredGetAllQuery>;
};
{=/ operations.GetAll =}
{=# operations.Create =}

export interface {= entityUpper =}Create {
  action: ActionFor<RegisteredCreateAction>;
  useAction: UseActionFor<RegisteredCreateAction>;
};
{=/ operations.Create =}
{=# operations.Update =}

export interface {= entityUpper =}Update {
  action: ActionFor<RegisteredUpdateAction>;
  useAction: UseActionFor<RegisteredUpdateAction>;
};
{=/ operations.Update =}
{=# operations.Delete =}

export interface {= entityUpper =}Delete {
  action: ActionFor<RegisteredDeleteAction>;
  useAction: UseActionFor<RegisteredDeleteAction>;
};
{=/ operations.Delete =}

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

