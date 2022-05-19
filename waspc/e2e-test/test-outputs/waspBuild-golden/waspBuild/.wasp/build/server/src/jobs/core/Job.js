/**
 * This is a definition of a job (think draft or invocable computation), not the running instance itself.
 * This can be submitted one or more times to be executed by some job executor via the same instance.
 * Once submitted, you get a SubmittedJob to track it later.
 */
export class Job {
  #jobName
  #executorName

  /**
   * @param {string} jobName - Job name, which should be unique per executor.
   * @param {string} executorName - The name of the executor that will run submitted jobs.
   */
  constructor(jobName, executorName) {
    this.#jobName = jobName
    this.#executorName = executorName
  }

  get jobName() {
    return this.#jobName
  }

  get executorName() {
    return this.#executorName
  }

  // NOTE: Subclasses must implement this method.
  delay(...args) {
    throw new Error('Subclasses must implement this method')
  }

  // NOTE: Subclasses must implement this method.
  async submit(...args) {
    throw new Error('Subclasses must implement this method')
  }
}
