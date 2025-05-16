import * as z from 'zod';

export const FlyRegionListSchema = z.array(
  z.object({
    Code: z.string(),
    Name: z.string(),
  }),
);

export const FlySecretListSchema = z.array(
  z.object({
    Name: z.string(),
  }),
);
