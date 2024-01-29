import { configureQueryClient } from 'wasp/rpc'

export default async function mySetupFunction(): Promise<void> {
  console.log('Setting up client...')
  configureQueryClient({
    defaultOptions: {
      queries: {
        refetchOnWindowFocus: false,
      },
    },
  })
}
