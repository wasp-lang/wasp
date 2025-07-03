import { defineEnvValidationSchema } from "wasp/env";
import * as z from "zod";

export const serverEnvValidationSchema = defineEnvValidationSchema(
  z.object({
    TEST_ENV_VAR: z.string({
      required_error: "TEST_ENV_VAR is required.",
    }),
  }),
);

export const clientEnvValidationSchema = defineEnvValidationSchema(
  z.object({
    REACT_APP_NAME: z.string().default("Kitchen Sink App"),
  }),
);
