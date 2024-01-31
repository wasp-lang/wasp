import { configureQueryClient } from "wasp/client/operations"

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
