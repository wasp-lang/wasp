import * as z from 'zod';
import { Prisma } from '@prisma/client';

import { SubmitProject, GetProjects } from 'wasp/server/operations';
import { HttpError } from 'wasp/server';
import { submitProjectInputSchema } from './schemas/projects';

export const submitProject: SubmitProject<z.infer<typeof submitProjectInputSchema>, void> = async (
  rawArgs,
  context,
) => {
  try {
    const { name, email, github, description, twitter, country, website, image } =
      submitProjectInputSchema.parse(rawArgs);

    await context.entities.Submission.create({
      data: {
        name,
        email,
        github,
        description,
        twitter,
        country,
        website,
        image,
        approved: process.env.HEADLESS_TESTING === 'true',
      },
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
};

export const getProjects = (async (_data, context) => {
  return context.entities.Submission.findMany({
    where: {
      approved: true,
    },
    orderBy: {
      createdAt: 'desc',
    },
  });
}) satisfies GetProjects<void>;
