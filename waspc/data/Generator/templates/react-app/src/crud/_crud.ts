{{={= =}=}}
import { createAction } from "../actions/core";
import { useAction } from "../actions";
import { createQuery } from "../queries/core";
import { useQuery } from "../queries";
import { {= entityUpper =} } from "../entities";

type EntityType = {= entityUpper =}
type PrimaryFieldType = {= entityUpper =}["{= primaryFieldName =}"]
type PrimaryFieldArgs = { {= primaryFieldName =}: PrimaryFieldType }
type CreateArgs = Partial<Omit<EntityType, "{= primaryFieldName =}">>

function createCrud() {
    {=# operationsData.Get =}
    {=# isEnabled =}
    const crudGetQuery = createQuery<(args: PrimaryFieldArgs) => Promise<EntityType>>(
        '{= route =}',
        {=& entitiesArray =}
    )
    {=/ isEnabled =}
    {=/ operationsData.Get =}
    {=# operationsData.GetAll =}
    {=# isEnabled =}
    const crudGetAllQuery = createQuery<() => Promise<EntityType[]>>(
        '{= route =}',
        {=& entitiesArray =}
    )
    {=/ isEnabled =}
    {=/ operationsData.GetAll =}
    {=# operationsData.Create =}
    {=# isEnabled =}
    const crudCreateAction = createAction<(args: CreateArgs) => Promise<void>>(
        '{= route =}',
        {=& entitiesArray =}
    )
    {=/ isEnabled =}
    {=/ operationsData.Create =}
    {=# operationsData.Update =}
    {=# isEnabled =}
    const crudUpdateAction = createAction<(args: CreateArgs & PrimaryFieldArgs) => Promise<void>>(
        '{= route =}',
        {=& entitiesArray =}
    )
    {=/ isEnabled =}
    {=/ operationsData.Update =}
    {=# operationsData.Delete =}
    {=# isEnabled =}
    const crudDeleteAction = createAction<(args: PrimaryFieldArgs) => Promise<void>>(
        '{= route =}',
        {=& entitiesArray =}
    )
    {=/ isEnabled =}
    {=/ operationsData.Delete =}
    return {
        {=# operationsData.Get.isEnabled =}
        get: {
            query: crudGetQuery,
            useQuery(args: PrimaryFieldArgs) {
                return useQuery(crudGetQuery, args);
            }
        },
        {=/ operationsData.Get.isEnabled =}
        {=# operationsData.GetAll.isEnabled =}
        getAll: {
            query: crudGetAllQuery,
            useQuery() {
                return useQuery(crudGetAllQuery);
            }
        },
        {=/ operationsData.GetAll.isEnabled =}
        {=# operationsData.Create.isEnabled =}
        create: {
            action: crudCreateAction,
            useAction() {
                return useAction(crudCreateAction);
            }
        },
        {=/ operationsData.Create.isEnabled =}
        {=# operationsData.Update.isEnabled =}
        update: {
            action: crudUpdateAction,
            useAction() {
                return useAction(crudUpdateAction);
            }
        },
        {=/ operationsData.Update.isEnabled =}
        {=# operationsData.Delete.isEnabled =}
        delete: {
            action: crudDeleteAction,
            useAction() {
                return useAction(crudDeleteAction);
            }
        },
        {=/ operationsData.Delete.isEnabled =}
    }
}

export const {= name =} = createCrud();
   