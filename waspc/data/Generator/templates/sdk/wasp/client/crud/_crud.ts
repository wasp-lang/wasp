{{={= =}=}}
import { createAction } from "../operations/actions/core.js";
import type { ActionFor } from "../operations/actions/core.js";
import { createQuery } from "../operations/queries/core.js";
import type { QueryFor } from "../operations/queries/core.js";
import { makeUseActionFor, makeUseQueryFor } from "./operationsHelpers.js";
import type { UseActionFor, UseQueryFor } from "./operationsHelpers.js";
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

{=# operations.Get =}
const _getQuery: QueryFor<GetQueryResolved> = createQuery<GetQueryResolved>(
    '{= fullPath =}',
    {=& entitiesArray =}
)
const _getUseQuery: UseQueryFor<GetQueryResolved> = makeUseQueryFor(_getQuery)
{=/ operations.Get =}
{=# operations.GetAll =}
const _getAllQuery: QueryFor<GetAllQueryResolved> = createQuery<GetAllQueryResolved>(
    '{= fullPath =}',
    {=& entitiesArray =}
)
const _getAllUseQuery: UseQueryFor<GetAllQueryResolved> = makeUseQueryFor(_getAllQuery)
{=/ operations.GetAll =}
{=# operations.Create =}
const _createAction: ActionFor<CreateActionResolved> = createAction<CreateActionResolved>(
    '{= fullPath =}',
    {=& entitiesArray =}
)
const _createUseAction: UseActionFor<CreateActionResolved> = makeUseActionFor(_createAction)
{=/ operations.Create =}
{=# operations.Update =}
const _updateAction: ActionFor<UpdateActionResolved> = createAction<UpdateActionResolved>(
    '{= fullPath =}',
    {=& entitiesArray =}
)
const _updateUseAction: UseActionFor<UpdateActionResolved> = makeUseActionFor(_updateAction)
{=/ operations.Update =}
{=# operations.Delete =}
const _deleteAction: ActionFor<DeleteActionResolved> = createAction<DeleteActionResolved>(
    '{= fullPath =}',
    {=& entitiesArray =}
)
const _deleteUseAction: UseActionFor<DeleteActionResolved> = makeUseActionFor(_deleteAction)
{=/ operations.Delete =}

// PUBLIC API
export const {= name =}: {
    {=# operations.Get =}
    get: {
        query: typeof _getQuery,
        useQuery: typeof _getUseQuery
    },
    {=/ operations.Get =}
    {=# operations.GetAll =}
    getAll: {
        query: typeof _getAllQuery,
        useQuery: typeof _getAllUseQuery
    },
    {=/ operations.GetAll =}
    {=# operations.Create =}
    create: {
        action: typeof _createAction,
        useAction: typeof _createUseAction
    },
    {=/ operations.Create =}
    {=# operations.Update =}
    update: {
        action: typeof _updateAction,
        useAction: typeof _updateUseAction
    },
    {=/ operations.Update =}
    {=# operations.Delete =}
    delete: {
        action: typeof _deleteAction,
        useAction: typeof _deleteUseAction
    },
    {=/ operations.Delete =}
} = {
    {=# operations.Get =}
    get: {
        query: _getQuery,
        useQuery: _getUseQuery
    },
    {=/ operations.Get =}
    {=# operations.GetAll =}
    getAll: {
        query: _getAllQuery,
        useQuery: _getAllUseQuery
    },
    {=/ operations.GetAll =}
    {=# operations.Create =}
    create: {
        action: _createAction,
        useAction: _createUseAction
    },
    {=/ operations.Create =}
    {=# operations.Update =}
    update: {
        action: _updateAction,
        useAction: _updateUseAction
    },
    {=/ operations.Update =}
    {=# operations.Delete =}
    delete: {
        action: _deleteAction,
        useAction: _deleteUseAction
    },
    {=/ operations.Delete =}
}
