import { QueryClient } from 'react-query'

let resolveQueryClientInitialized, isQueryClientSetUp
export const queryClientInitialized = new Promise(resolve => {
  resolveQueryClientInitialized = resolve
});

export function setupQueryClient(config = {}) {
  if (isQueryClientSetUp) {
    return
  }

  const queryClient = new QueryClient(config)
  isQueryClientSetUp = true;
  resolveQueryClientInitialized(queryClient)
}