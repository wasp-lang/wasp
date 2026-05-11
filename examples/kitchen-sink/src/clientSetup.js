import { configureQueryClient, testingAction } from "wasp/client/operations";

export function setup() {
  console.log("This was called from the client setup function");

  if (!import.meta.env.SSR) {
    testingAction();
  }

  // Configure the React Query client
  configureQueryClient({
    defaultOptions: {
      queries: {
        refetchOnWindowFocus: false,
      },
    },
  });
}
