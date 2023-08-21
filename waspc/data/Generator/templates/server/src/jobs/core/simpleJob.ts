import { sleep } from "../../utils.js";
import { Job } from "./Job.js";
import { createSubmittedJob } from "./SubmittedJob.js";

export const SIMPLE_EXECUTOR_NAME = Symbol("Simple");

/**
 * A simple job mainly intended for testing. It will not submit work to any
 * job executor, but instead will simply invoke the underlying perform function.
 * It does not support `schedule`. It does not require any extra NPM dependencies
 * or infrastructure, however.
 */
interface SimpleJob<Input, Output extends object, Entities> extends Job<Input, Output, Entities> {
  // delaySeconds - The number of seconds to delay invoking the Job function.
  delaySeconds: number;
}

export function createJob<Input, Output extends object, Entities>({
  jobName,
  jobFn,
  delaySeconds = 0,
}: {
  jobName: SimpleJob<Input, Output, Entities>["jobName"];
  jobFn: SimpleJob<Input, Output, Entities>["jobFn"];
  delaySeconds?: SimpleJob<Input, Output, Entities>["delaySeconds"];
}): SimpleJob<Input, Output, Entities> {
  return {
    jobName,
    executorName: SIMPLE_EXECUTOR_NAME,
    jobFn,
    delaySeconds: delaySeconds,
    // delaySeconds - Used to delay the processing of the job by some number of seconds.
    delay(delaySeconds: number) {
      return createJob<Input, Output, Entities>({ jobName, jobFn, delaySeconds });
    },
    async submit(jobArgs: any[]) {
      await sleep(this.delaySeconds * 1000);
      await this.jobFn(jobArgs);
      // NOTE: Dumb random ID generator, mainly so we don't have to add `uuid`
      // as a dependency in the server generator for something nobody will likely use.
      const jobId = (Math.random() + 1).toString(36).substring(7);
      return createSubmittedJob<Input, Output, Entities>(this, jobId);
    },
  };
}
