import { defineUserSignupFields } from "wasp/auth/providers/types";
import { z } from "zod";

const emailDataSchema = z.object({
  email: z.string(),
});

export const getEmailUserFields = defineUserSignupFields({
  email: (data) => emailDataSchema.parse(data).email,
});
