import * as z from "zod";

export const fileUploadEnvSchema = z.object({
  AWS_S3_REGION: z.string({
    error: "AWS_S3_REGION is required for file uploads",
  }),
  AWS_S3_IAM_ACCESS_KEY: z.string({
    error: "AWS_S3_IAM_ACCESS_KEY is required for file uploads",
  }),
  AWS_S3_IAM_SECRET_KEY: z.string({
    error: "AWS_S3_IAM_SECRET_KEY is required for file uploads",
  }),
  AWS_S3_FILES_BUCKET: z.string({
    error: "AWS_S3_FILES_BUCKET is required for file uploads",
  }),
});
