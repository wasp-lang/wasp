import { UppercaseTextRequestState } from "@prisma/client";
import { UppercaseTextRequest } from "wasp/entities";
import { HttpError } from "wasp/server";
import { uppercaseTextJob, UppercaseTextJob } from "wasp/server/jobs";
import {
  GetTextUppercaseRequests,
  RequestUppercaseText,
} from "wasp/server/operations";
import { sleep } from "./sleep";

export const requestUppercaseText: RequestUppercaseText<{
  text: string;
}> = async (args, context) => {
  if (!context.user) {
    throw new HttpError(401, "Unauthorized");
  }

  const { text } = args;

  // Save the request to the database
  const request = await context.entities.UppercaseTextRequest.create({
    data: {
      input: text,
      state: UppercaseTextRequestState.PENDING,
      user: {
        connect: { id: context.user.id },
      },
    },
  });

  // Submit the job for processing
  uppercaseTextJob.submit({
    requestId: request.id,
  });

  return { requestId: request.id };
};

export const getTextUppercaseRequests: GetTextUppercaseRequests<
  never,
  UppercaseTextRequest[]
> = (_args, context) => {
  if (!context.user) {
    throw new HttpError(401, "Unauthorized");
  }
  return context.entities.UppercaseTextRequest.findMany({
    where: { userId: context.user.id },
    orderBy: { createdAt: "desc" },
  });
};

export const uppercaseText: UppercaseTextJob<
  { requestId: string },
  void
> = async (args, context) => {
  const { requestId } = args;
  const request = await context.entities.UppercaseTextRequest.findFirstOrThrow({
    where: { id: requestId },
  });

  try {
    await sleep(2000); // Simulate a delay of 2 seconds
    const uppercaseText = request.input.toUpperCase();
    await context.entities.UppercaseTextRequest.update({
      where: { id: requestId },
      data: { output: uppercaseText, state: UppercaseTextRequestState.SUCCESS },
    });
  } catch (error) {
    // This is just to demonstrate error handling in a job
    await context.entities.UppercaseTextRequest.update({
      where: { id: requestId },
      data: { state: UppercaseTextRequestState.ERROR },
    });
    throw error;
  }
};
