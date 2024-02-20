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
} from './core'

export {
    // PUBLIC API
    configureQueryClient,
    // PRIVATE API (framework code)
    initializeQueryClient,
    // PRIVATE API (framework code)
    queryClientInitialized
} from './queryClient'
