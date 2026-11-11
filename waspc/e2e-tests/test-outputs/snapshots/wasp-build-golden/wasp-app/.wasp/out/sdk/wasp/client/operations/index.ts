// PUBLIC API
export * from './actions'
// MOSTLY PUBLIC API (see the file for details)
export * from './queries'

export {
    // PUBLIC API
    useAction,
    // PUBLIC API
    useQuery,
    // PUBLIC API
    type OptimisticUpdateDefinition,
} from '@wasp/lib-sdk-core/browser'

export {
    // PUBLIC API
    type CreateQueryClientFn,
} from '@wasp/lib-sdk-core/browser'

// PRIVATE API (framework code)
export { queryClient } from './queryClient'

export type {
    // PUBLIC API
    QueryMetadata,
} from '@wasp/lib-sdk-core'
