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
} from './hooks'

export {
    // PUBLIC API
    configureQueryClient,
} from './queryClient'
