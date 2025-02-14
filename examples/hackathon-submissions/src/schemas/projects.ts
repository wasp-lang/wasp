import * as z from 'zod';

export const projectCreateSchema = z
  .object({
    name: z.string({
      required_error: 'Name is required',
    }),
    email: z.string({
      required_error: 'Email is required',
    }),
    github: z.string({
      required_error: 'Github Repo is required',
    }),
    description: z.string({
      required_error: 'Description is required',
    }),
    twitter: z.string().optional(),
    country: z.string().optional(),
    website: z.string().optional(),
    image: z.string().optional(),
  })
  .strict();
