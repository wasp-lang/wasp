import * as z from 'zod';
import { Prisma } from '@prisma/client';

import { SubmitProject, GetProjects } from 'wasp/server/operations';
import { projectCreateSchema } from './schemas/projects';
import { HttpError } from 'wasp/server';

export const submitProject = (async (data, context) => {
  try {
    const validatedData = projectCreateSchema.parse(data);
    await context.entities.Submission.create({
      data: validatedData,
    });
  } catch (e) {
    if (e instanceof Prisma.PrismaClientKnownRequestError) {
      throw new HttpError(400, 'There is already a submission with this name or email.');
    } else if (e instanceof z.ZodError) {
      throw new HttpError(400, `Validation error: ${e.errors[0].message}`);
    } else {
      throw new HttpError(500, 'Internal Server Error');
    }
  }
}) satisfies SubmitProject<z.infer<typeof projectCreateSchema>>;

export const getProjects = (async (_data, context) => {
  return context.entities.Submission.findMany({
    where: {
      approved: true,
    },
    orderBy: {
      createdAt: 'desc',
    },
  });
}) satisfies GetProjects;
