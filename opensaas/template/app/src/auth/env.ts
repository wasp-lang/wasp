import * as z from "zod";

export const authEnvSchema = z.object({
  ADMIN_EMAILS: z
    .string()
    .default("")
    .transform((val) =>
      val
        .split(",")
        .map((email) => email.trim())
        .filter(Boolean),
    ),
});
