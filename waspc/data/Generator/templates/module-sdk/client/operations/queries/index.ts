{{={= =}=}}
{=# hasQueries =}
import { type QueryFor, createQuery } from './core';
{=# queries =}
import type { {= typeName =} } from 'wasp/server/operations/queries/types';
{=/ queries =}

{=# queries =}
export const {= name =}: QueryFor<{= typeName =}> = createQuery<{= typeName =}>('{= name =}', {=& entitiesArray =});
{=/ queries =}

export { useQuery } from './core';
{=/ hasQueries =}
{=^ hasQueries =}
export {};
{=/ hasQueries =}
