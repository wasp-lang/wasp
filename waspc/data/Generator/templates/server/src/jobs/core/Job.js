/**
 * This is a definition of a job or computation, not the running instance itself.
 * This can be submitted once or more to be executed by some job executor.
 * Once submitted, you get a SubmittedJob to track it later.
 */
export class Job {
  #jobName
  #delaySeconds = 0

  /**
   * @param {string} jobName - The user-defined name used when creating a job in the template.
   * @param {int} delaySeconds - Used to delay `submit()` calls when creating a new object in `delay()`.
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
