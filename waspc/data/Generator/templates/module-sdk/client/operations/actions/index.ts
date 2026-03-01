{{={= =}=}}
{=# hasActions =}
import { type ActionFor, createAction } from './core';
{=# actions =}
import type { {= typeName =} } from 'wasp/server/operations/actions/types';
{=/ actions =}

{=# actions =}
export const {= name =}: ActionFor<{= typeName =}> = createAction<{= typeName =}>('{= name =}', {=& entitiesArray =});
{=/ actions =}

export { useAction } from './core';
{=/ hasActions =}
{=^ hasActions =}
export {};
{=/ hasActions =}
