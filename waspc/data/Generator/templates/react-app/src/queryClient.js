import { QueryClient } from 'react-query'

let resolveQueryClientInitialized
export const queryClientInitialized = new Promise(resolve => {
  resolveQueryClientInitialized = resolve
});

export function setupQueryClient(config = {}) {
  const queryClient = new QueryClient(config)
  resolveQueryClientInitialized(queryClient)
}