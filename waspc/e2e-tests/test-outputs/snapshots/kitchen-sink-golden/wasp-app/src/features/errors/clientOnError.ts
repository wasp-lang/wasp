import { type ClientOnErrorFn } from "wasp/client";
import { getErrorMessage } from "wasp/errors";

export const onClientError: ClientOnErrorFn = (error, context) => {
  console.info(
    `client.onError got an error from ${context.source}: ${getErrorMessage(error)}`,
  );
};
