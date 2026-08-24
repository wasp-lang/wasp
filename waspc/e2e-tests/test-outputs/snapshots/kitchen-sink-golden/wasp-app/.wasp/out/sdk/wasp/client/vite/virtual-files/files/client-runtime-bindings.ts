import { initializeClientRuntime } from "wasp/client/runtime";

import { clientEnvValidationSchema as clientEnvValidationSchema_ext } from './src/env'

initializeClientRuntime({
  clientEnvValidationSchema: clientEnvValidationSchema_ext,
});
