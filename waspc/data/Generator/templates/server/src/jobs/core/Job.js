/**
 * This is a definition of a job (think draft or future computation), not the running instance itself.
 * This can be submitted once or more to be executed by some job executor.
 * Once submitted, you get a SubmittedJob to track it later.
 */
export class Job {
  #jobName
  #delaySeconds = 0

  /**
   * @param {string} jobName - The user-defined name used when creating a job in the generated template.
   * @param {int} delaySeconds - Used to delay perform function calls.
   */
  constructor(jobName, delaySeconds) {
    this.#jobName = jobName
    this.#delaySeconds = delaySeconds
  }

  jobName() {
    return this.#jobName
  }

  delaySeconds() {
    return this.#delaySeconds
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
