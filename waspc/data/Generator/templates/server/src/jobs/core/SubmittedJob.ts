import type { Job } from "./Job";

/**
 * This is the result of submitting a Job to some executor.
 * It can be used by callers to track things, or call executor-specific subclass functionality.
 */
export interface SubmittedJob<Input, Output extends object, Entities> {
  // job - The Job that submitted work to an executor.
  job: Job<Input, Output, Entities>;
  // jobId - A UUID for a submitted job in that executor's ecosystem.
  jobId: string;
  jobName: string;
  executorName: string | symbol;
}

export function createSubmittedJob<Input, Output extends object, Entities>(job: Job<Input, Output, Entities>, jobId: string) {
  return {
    job,
    jobId,
    jobName: job.jobName,
    executorName: job.executorName,
  } satisfies SubmittedJob<Input, Output, Entities>;
}
