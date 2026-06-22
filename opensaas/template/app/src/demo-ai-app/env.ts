import * as z from "zod";

export const demoAiAppEnvSchema = z.object({
  OPENAI_API_KEY: z.string({
    error: "OPENAI_API_KEY is required for the demo AI app",
  }),
});
