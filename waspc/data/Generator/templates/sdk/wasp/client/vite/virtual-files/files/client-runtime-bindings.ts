{{={= =}=}}
import { initializeClientRuntime } from "wasp/client/runtime";

{=# clientEnvValidationSchema.isDefined =}
{=& clientEnvValidationSchema.importStatement =}
{=/ clientEnvValidationSchema.isDefined =}

initializeClientRuntime({
  clientEnvValidationSchema: {=# clientEnvValidationSchema.isDefined =}{= clientEnvValidationSchema.importIdentifier =}{=/ clientEnvValidationSchema.isDefined =}{=^ clientEnvValidationSchema.isDefined =}undefined{=/ clientEnvValidationSchema.isDefined =},
});
