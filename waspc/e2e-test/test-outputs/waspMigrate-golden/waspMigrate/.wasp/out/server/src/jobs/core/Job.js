/**
 * This is a definition of a job (think draft or invocable computation), not the running instance itself.
 * This can be submitted one or more times to be executed by some job executor via the same instance.
 * Once submitted, you get a SubmittedJob to track it later.
 */
export class Job {
  #jobName

  /**
   * @param {string} jobName - The user-defined name used when creating a job in the generated template.
   */
  constructor(jobName) {
    this.#jobName = jobName
  }

  jobName() {
    return this.#jobName
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
