import { JSONValue } from '../../_types/serialization.js'
/**
 * This is a definition of a job (think draft or invocable computation), not the running instance itself.
 * This can be submitted one or more times to be executed by some job executor via the same instance.
 * Once submitted, you get a SubmittedJob to track it later.
 */
export interface Job<
  Input extends object,
  Output extends JSONValue | void,
  Entities
> {
  // jobName - Job name, which should be unique per executor.
  jobName: string
  // executorName - The name of the executor that will run submitted jobs.
  executorName: string | symbol
  jobFn: (
    data: Input,
    context: { entities: Entities }
  ) => Promise<Output>
  delay(...args: any[]): Job<Input, Output, Entities>
  submit(...args: any[]): Promise<SubmittedJob<Input, Output, Entities>>
}

export function createJob<
  Input extends object,
  Output extends JSONValue | void,
  Entities
>(
  jobName: string,
  executorName: string | symbol
): Job<Input, Output, Entities> {
  return {
    jobName,
    executorName,
    jobFn: async () => {
      throw new Error('Subclasses must implement this method')
    },
    delay() {
      throw new Error('Subclasses must implement this method')
    },
    async submit() {
      throw new Error('Subclasses must implement this method')
    },
  } satisfies Job<Input, Output, Entities>
}

/**
 * This is the result of submitting a Job to some executor.
 * It can be used by callers to track things, or call executor-specific subclass functionality.
 */
export interface SubmittedJob<
  Input extends object,
  Output extends JSONValue | void,
  Entities
> {
  // job - The Job that submitted work to an executor.
  job: Job<Input, Output, Entities>
  // jobId - A UUID for a submitted job in that executor's ecosystem.
  jobId: string
  jobName: string
  executorName: string | symbol
}

export function createSubmittedJob<
  Input extends object,
  Output extends JSONValue | void,
  Entities
>(job: Job<Input, Output, Entities>, jobId: string) {
  return {
    job,
    jobId,
    jobName: job.jobName,
    executorName: job.executorName,
  } satisfies SubmittedJob<Input, Output, Entities>
}
