{{={= =}=}}
import { createAction } from "../actions/core";
import { useAction } from "../actions";
import { createQuery } from "../queries/core";
import { useQuery } from "../queries";
import { {= entityUpper =} } from "../entities";
import { RouteInputs, EntityType } from "../universal/crud/{= name =}";

function createCrud() {
    {=# operations.Get =}
    {=# isEnabled =}
    const crudGetQuery = createQuery<(args: RouteInputs.Get) => Promise<EntityType>>(
        '{= fullPath =}',
        {=& entitiesArray =}
    )
    {=/ isEnabled =}
    {=/ operations.Get =}
    {=# operations.GetAll =}
    {=# isEnabled =}
    const crudGetAllQuery = createQuery<() => Promise<EntityType[]>>(
        '{= fullPath =}',
        {=& entitiesArray =}
    )
    {=/ isEnabled =}
    {=/ operations.GetAll =}
    {=# operations.Create =}
    {=# isEnabled =}
    const crudCreateAction = createAction<(args: RouteInputs.Create) => Promise<void>>(
        '{= fullPath =}',
        {=& entitiesArray =}
    )
    {=/ isEnabled =}
    {=/ operations.Create =}
    {=# operations.Update =}
    {=# isEnabled =}
    const crudUpdateAction = createAction<(args: RouteInputs.Update) => Promise<void>>(
        '{= fullPath =}',
        {=& entitiesArray =}
    )
    {=/ isEnabled =}
    {=/ operations.Update =}
    {=# operations.Delete =}
    {=# isEnabled =}
    const crudDeleteAction = createAction<(args: RouteInputs.Delete) => Promise<void>>(
        '{= fullPath =}',
        {=& entitiesArray =}
    )
    {=/ isEnabled =}
    {=/ operations.Delete =}
    return {
        {=# operations.Get.isEnabled =}
        get: {
            query: crudGetQuery,
            useQuery(args: RouteInputs.Get) {
                return useQuery(crudGetQuery, args);
            }
        },
        {=/ operations.Get.isEnabled =}
        {=# operations.GetAll.isEnabled =}
        getAll: {
            query: crudGetAllQuery,
            useQuery() {
                return useQuery(crudGetAllQuery);
            }
        },
        {=/ operations.GetAll.isEnabled =}
        {=# operations.Create.isEnabled =}
        create: {
            action: crudCreateAction,
            useAction() {
                return useAction(crudCreateAction);
            }
        },
        {=/ operations.Create.isEnabled =}
        {=# operations.Update.isEnabled =}
        update: {
            action: crudUpdateAction,
            useAction() {
                return useAction(crudUpdateAction);
            }
        },
        {=/ operations.Update.isEnabled =}
        {=# operations.Delete.isEnabled =}
        delete: {
            action: crudDeleteAction,
            useAction() {
                return useAction(crudDeleteAction);
            }
        },
        {=/ operations.Delete.isEnabled =}
    }
}

export const {= name =} = createCrud();
