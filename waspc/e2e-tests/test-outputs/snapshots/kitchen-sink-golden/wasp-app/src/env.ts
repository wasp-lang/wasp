import { defineEnvValidationSchema } from "wasp/env";
import * as z from "zod";

export const serverEnvValidationSchema = defineEnvValidationSchema(
  z.object({
    TEST_ENV_VAR: z.string({
      required_error: "TEST_ENV_VAR is required.",
    }).min(50),
  }),
);

export const clientEnvValidationSchema = defineEnvValidationSchema(
  z.object({
    REACT_APP_NAME: z.string().default("Kitchen Sink App"),
    REACT_APP_MUST_BE_DEFINED: z.string({
      required_error: "REACT_APP_MUST_BE_DEFINED string is required.",
    }),
    REACT_APP_MUST_BE_LONG: z.string().min(10),
  }),
);
