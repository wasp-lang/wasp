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
    this.delayMs = 0
    this.jobName = 'unknown'
    Object.assign(this, values)
  }

  delay(ms) {
    return new PgBossJobFactory({ ...this, delayMs: ms })
  }

  async performAsync(payload) {
    const delaySeconds = (this.delayMs > 0) ? Math.trunc(this.delayMs / 1000) : 0
    const jobId = await boss.send(this.jobName, payload, { startAfter: delaySeconds, onComplete: true })
    const result = new Promise((resolve, _reject) => {
      pgBossCompletionEventEmitter.on(this.jobName, job => {
        if (job.data.request.id === jobId) {
          resolve(job.data.response)
        }
      })
    })
    return { result, jobId }
  }
}

export async function jobFactory(jobName, fn) {
  boss.onComplete(jobName, job => {
    pgBossCompletionEventEmitter.emit(jobName, job)
  })
  await boss.work(jobName, fn)
  return new PgBossJobFactory({ perform: fn, jobName })
}
