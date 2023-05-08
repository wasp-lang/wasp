{{={= =}=}}
import { createAction } from "../actions/core";
import { createQuery } from "../queries/core";

// TODO: use a helper function "createCrud" 
export const {= name =} = {
    {=# enabledOperations.Get =}
    {=# operationsData.Get =}
    get: createQuery(
        '{= route =}',
        {=& entitiesArray =}
    ),
    {=/ operationsData.Get =}
    {=/ enabledOperations.Get =}
    {=# enabledOperations.GetAll =}
    {=# operationsData.GetAll =}
    getAll: createQuery(
        '{= route =}',
        {=& entitiesArray =}
    ),
    {=/ operationsData.GetAll =}
    {=/ enabledOperations.GetAll =}
    {=# enabledOperations.Create =}
    {=# operationsData.Create =}
    create: createAction(
        '{= route =}',
        {=& entitiesArray =}
    ),
    {=/ operationsData.Create =}
    {=/ enabledOperations.Create =}
    {=# enabledOperations.Update =}
    {=# operationsData.Update =}
    update: createAction(
        '{= route =}',
        {=& entitiesArray =}
    ),
    {=/ operationsData.Update =}
    {=/ enabledOperations.Update =}
    {=# enabledOperations.Delete =}
    {=# operationsData.Delete =}
    delete: createAction(
        '{= route =}',
        {=& entitiesArray =}
    ),
    {=/ operationsData.Delete =}
    {=/ enabledOperations.Delete =}
}