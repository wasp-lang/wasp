import type * as z from "zod";

export type ClientRuntimeBindings = {
  clientEnvValidationSchema?: z.ZodObject;
};

let clientRuntimeBindings: ClientRuntimeBindings | undefined;

export function initializeClientRuntime(bindings: ClientRuntimeBindings): void {
  if (clientRuntimeBindings) {
    throw new Error("Wasp client runtime is already initialized");
  }

  clientRuntimeBindings = bindings;
}

export function getClientEnvValidationSchema():
  | ClientRuntimeBindings["clientEnvValidationSchema"]
  | undefined {
  if (!clientRuntimeBindings) {
    throw new Error(
      "Wasp client runtime is not initialized (while accessing client environment validation schema)",
    );
  }

  return clientRuntimeBindings.clientEnvValidationSchema;
}
