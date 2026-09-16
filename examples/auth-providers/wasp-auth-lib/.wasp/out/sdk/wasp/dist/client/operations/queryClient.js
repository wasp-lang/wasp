import { QueryClient } from '@tanstack/react-query';
const defaultQueryClientConfig = {};
let queryClientConfig, resolveQueryClientInitialized, isQueryClientInitialized;
// PRIVATE API (framework code)
export const queryClientInitialized = new Promise((resolve) => {
    resolveQueryClientInitialized = resolve;
});
// PUBLIC API
export function configureQueryClient(config) {
    if (isQueryClientInitialized) {
        throw new Error("Attempted to configure the QueryClient after initialization");
    }
    queryClientConfig = config;
}
// PRIVATE API (framework code)
export function initializeQueryClient() {
    const queryClient = new QueryClient(queryClientConfig ?? defaultQueryClientConfig);
    isQueryClientInitialized = true;
    resolveQueryClientInitialized(queryClient);
}
//# sourceMappingURL=queryClient.js.map