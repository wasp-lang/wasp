import { initializeQueryClient } from '../operations/queryClient'


// PRIVATE API (framework code)
// The QueryClient must be created only after the setup function completes,
// because the setup function may call `configureQueryClient`.
export const queryClient = initializeQueryClient()
