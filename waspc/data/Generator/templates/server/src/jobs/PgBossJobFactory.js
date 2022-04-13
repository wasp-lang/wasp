import PgBoss from 'pg-boss';

export const boss = new PgBoss(process.env.DATABASE_URL);

export async function startPgBoss() {
  console.log('Starting Pg Boss...')
  boss.on('error', error => console.error(error))
  await boss.start()
  console.log('Pg Boss started!')
}

class PgBossJobFactory {
  constructor(values) {
    this.perform = () => { }
    this.delayMs = 0
    this.queue = 'default'
    Object.assign(this, values)
  }

  delay(ms) {
    return new PgBossJobFactory({ ...this, delayMs: ms })
  }

  performAsync(args) {
    return {
      result: boss.send(this.queue, args)
    }
  }
}

export function jobFactory(fn) {
  const queue = 'TODO' // TODO: make param
  boss.work(queue, fn)
  return new PgBossJobFactory({ perform: fn, queue })
}
