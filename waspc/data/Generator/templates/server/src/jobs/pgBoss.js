import PgBoss from 'pg-boss'

export const boss = new PgBoss({ connectionString: process.env.DATABASE_URL })

export async function startPgBoss() {
  console.log('Starting PgBoss...')
  boss.on('error', error => console.error(error))
  await boss.start()
  console.log('PgBoss started!')
}
