import { clientSetup as clientSetup_ext } from 'wasp/src/clientSetup'
import { initializeQueryClient } from '../operations/queryClient'

// The user-defined setup function must run before anything else in the app.
// Since this is a top-level await, every module that (transitively) imports
// this one waits for it to finish before executing its own code.
await clientSetup_ext()

// PRIVATE API (framework code)
// The QueryClient must be created only after the setup function completes,
// because the setup function may call `configureQueryClient`.
export const queryClient = initializeQueryClient()
