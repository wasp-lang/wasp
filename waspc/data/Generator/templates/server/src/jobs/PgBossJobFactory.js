import { EventEmitter } from 'events'
import PgBoss from 'pg-boss'

export const boss = new PgBoss(process.env.DATABASE_URL)

export async function startPgBoss() {
  console.log('Starting PgBoss...')
  boss.on('error', error => console.error(error))
  await boss.start()
  console.log('PgBoss started!')
}

const pgBossCompletionEventEmitter = new EventEmitter()

class PgBossJobFactory {
  constructor(values) {
    this.perform = () => { }
    this.options = {}
    this.startAfter = 0
    this.jobName = 'unknown'
    Object.assign(this, values)
  }

  delay(startAfter) {
    return new PgBossJobFactory({ ...this, startAfter })
  }

  async performAsync(payload) {
    const jobId = await boss.send(this.jobName, payload, { ...this.options, startAfter: this.startAfter })
    return {
      jobType: 'pg-boss',
      jobName: this.jobName,
      jobId,
      async cancel() { return boss.cancel(jobId) },
      async resume() { return boss.resume(jobId) },
      async details() { return boss.getJobById(jobId) }
    }
  }
}

export async function jobFactory(jobName, fn, options) {
  await boss.work(jobName, fn)
  return new PgBossJobFactory({ perform: fn, jobName, options })
}
