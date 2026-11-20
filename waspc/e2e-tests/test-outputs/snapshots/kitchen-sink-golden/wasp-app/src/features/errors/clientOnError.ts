import { type ClientOnErrorFn } from "wasp/client";

export const onClientError: ClientOnErrorFn = (error, context) => {
  console.info(
    `client.onError got an error from ${context.source}: ${error instanceof Error ? error.message : String(error)}`,
  );
};
