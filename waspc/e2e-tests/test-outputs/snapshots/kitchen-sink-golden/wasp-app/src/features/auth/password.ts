import { definePasswordValidationSchema } from "wasp/auth";
import * as z from "zod";

export const passwordValidationSchema = definePasswordValidationSchema(
  z
    .string()
    .min(8, "Password must be at least 8 characters")
    .max(64, "Password must be at most 64 characters"),
);
