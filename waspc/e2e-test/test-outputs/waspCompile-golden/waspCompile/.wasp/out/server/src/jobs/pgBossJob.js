import { boss } from './pgBoss.js'
import { SubmittedJob } from './SubmittedJob.js'

class PgBossSubmittedJob extends SubmittedJob {
  constructor(jobName, jobId, executor, pgBoss) {
    super(jobName, jobId, executor)
    this.pgBoss = pgBoss
  }
}

class PgBossJob {
  constructor(values) {
    this.perform = () => { }
    this.options = {}
    this.startAfter = 0
    this.jobName = 'unknown'
    Object.assign(this, values)
  }

  delay(startAfter) {
    return new PgBossJob({ ...this, startAfter })
  }

  async submit(payload, options) {
    const jobId = await boss.send(this.jobName, payload, { ...this.options, startAfter: this.startAfter, ...options })
    return new PgBossSubmittedJob(this.jobName, jobId, 'PgBoss', {
      async cancel() { return boss.cancel(jobId) },
      async resume() { return boss.resume(jobId) },
      async details() { return boss.getJobById(jobId) }
    })
  }
}

export async function createJob(jobName, fn, options) {
  await boss.work(jobName, fn)
  return new PgBossJob({ perform: fn, jobName, options })
}
