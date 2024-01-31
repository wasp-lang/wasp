{{={= =}=}}
import { useAction, useQuery } from "wasp/client/operations";
import { createAction } from "../operations/actions/core.js";
import { createQuery } from "../operations/queries/core.js";
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

function createCrud() {
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
            useQuery(args: Parameters<GetQueryResolved>[0]) {
                return useQuery(crudGetQuery, args);
            }
        },
        {=/ operations.Get =}
        {=# operations.GetAll =}
        getAll: {
            query: crudGetAllQuery,
            useQuery() {
                return useQuery(crudGetAllQuery);
            }
        },
        {=/ operations.GetAll =}
        {=# operations.Create =}
        create: {
            action: crudCreateAction,
            useAction() {
                return useAction(crudCreateAction);
            }
        },
        {=/ operations.Create =}
        {=# operations.Update =}
        update: {
            action: crudUpdateAction,
            useAction() {
                return useAction(crudUpdateAction);
            }
        },
        {=/ operations.Update =}
        {=# operations.Delete =}
        delete: {
            action: crudDeleteAction,
            useAction() {
                return useAction(crudDeleteAction);
            }
        },
        {=/ operations.Delete =}
    }
}

// PUBLIC API
export const {= name =} = createCrud();
