{{={= =}=}}
// Development-only routes for the API Playground panel of Wasp DevTools. They
// record the requests the server handles, and let the panel submit jobs and
// inspect their runs. Never generated for production builds.
import express, { type NextFunction, type Request, type Response } from 'express'
import { prisma } from 'wasp/server'
{=# areThereAnyJobs =}
import * as jobs from 'wasp/server/jobs'
{=/ areThereAnyJobs =}
{=^ areThereAnyJobs =}
// The app has no jobs, so Wasp doesn't generate `wasp/server/jobs`.
const jobs = {}
{=/ areThereAnyJobs =}

const MAX_RECORDED_REQUESTS = 300
const MAX_RECORDED_BODY_LENGTH = 64 * 1024

type RecordedRequest = {
  id: number
  startedAt: string
  durationMs: number
  method: string
  path: string
  query: Record<string, unknown>
  status: number
  requestHeaders: Record<string, string>
  responseHeaders: Record<string, string>
  requestBody: unknown
  responseBody: string | null
  responseSize: number
  user: { id: string; identity: string | null } | null
}

const recordedRequests: RecordedRequest[] = []
const subscribers = new Set<Response>()
let nextRequestId = 1

/**
 * Records every request the server handles, except the DevTools' own, so the
 * API Playground can show them live.
 */
export function recordRequests(req: Request, res: Response, next: NextFunction) {
  // Skips Wasp's own dev-only routes, like this one and Wasp Studio's data.
  if (req.path.startsWith('/__wasp/') || req.method === 'OPTIONS') {
    return next()
  }

  const startedAt = new Date()
  const start = process.hrtime.bigint()
  const chunks: Buffer[] = []
  let capturedLength = 0

  const originalWrite = res.write.bind(res) as (...args: any[]) => boolean
  const originalEnd = res.end.bind(res) as (...args: any[]) => Response
  const capture = (chunk: unknown, encoding?: unknown) => {
    if (chunk === undefined || chunk === null || typeof chunk === 'function') return
    const buffer = Buffer.isBuffer(chunk)
      ? chunk
      : Buffer.from(String(chunk), typeof encoding === 'string' ? (encoding as BufferEncoding) : 'utf8')
    capturedLength += buffer.length
    if (capturedLength <= MAX_RECORDED_BODY_LENGTH) chunks.push(buffer)
  }
  ;(res as any).write = (chunk: unknown, ...rest: unknown[]) => {
    capture(chunk, rest[0])
    return originalWrite(chunk, ...rest)
  }
  ;(res as any).end = (chunk: unknown, ...rest: unknown[]) => {
    capture(chunk, rest[0])
    return originalEnd(chunk, ...rest)
  }

  res.on('finish', () => {
    const durationMs = Number(process.hrtime.bigint() - start) / 1e6
    const body = Buffer.concat(chunks).toString('utf8')
    const entry: RecordedRequest = {
      id: nextRequestId++,
      startedAt: startedAt.toISOString(),
      durationMs: Math.round(durationMs * 100) / 100,
      method: req.method,
      path: req.originalUrl.split('?')[0] ?? req.originalUrl,
      query: req.query as Record<string, unknown>,
      status: res.statusCode,
      requestHeaders: flattenHeaders(req.headers),
      responseHeaders: flattenHeaders(res.getHeaders()),
      requestBody: req.body ?? null,
      responseBody:
        capturedLength > MAX_RECORDED_BODY_LENGTH
          ? body + `\n… (${capturedLength - MAX_RECORDED_BODY_LENGTH} more bytes)`
          : body || null,
      responseSize: capturedLength,
      user: describeUser((req as any).user),
    }
    recordedRequests.push(entry)
    if (recordedRequests.length > MAX_RECORDED_REQUESTS) recordedRequests.shift()
    const message = `data: ${JSON.stringify(entry)}\n\n`
    for (const subscriber of subscribers) subscriber.write(message)
  })

  next()
}

export const devtoolsRouter = express.Router()

devtoolsRouter.get('/requests', (_req, res) => {
  res.json(recordedRequests)
})

devtoolsRouter.delete('/requests', (_req, res) => {
  recordedRequests.length = 0
  res.json({ ok: true })
})

devtoolsRouter.get('/requests/stream', (req, res) => {
  res.writeHead(200, {
    'Content-Type': 'text/event-stream',
    'Cache-Control': 'no-cache',
    Connection: 'keep-alive',
  })
  res.write(': connected\n\n')
  subscribers.add(res)
  const keepAlive = setInterval(() => res.write(': ping\n\n'), 15000)
  req.on('close', () => {
    clearInterval(keepAlive)
    subscribers.delete(res)
  })
})

devtoolsRouter.post('/jobs/:name/submit', async (req, res) => {
  const job = getJob(req.params.name)
  if (!job) {
    return res.status(404).json({ message: `No job named "${req.params.name}".` })
  }
  try {
    const { args = {}, delaySeconds } = req.body ?? {}
    const definition = delaySeconds ? job.delay(Number(delaySeconds)) : job
    const submitted = await definition.submit(args)
    res.json({ jobId: submitted.jobId, jobName: job.jobName, submittedAt: new Date().toISOString() })
  } catch (error) {
    res.status(500).json({ message: getErrorMessage(error) })
  }
})

devtoolsRouter.get('/jobs/:name/runs', async (req, res) => {
  if (!getJob(req.params.name)) {
    return res.status(404).json({ message: `No job named "${req.params.name}".` })
  }
  try {
    // pg-boss keeps active jobs in `pgboss.job` and moves finished ones to
    // `pgboss.archive` after a while.
    const runs = await prisma.$queryRawUnsafe<unknown[]>(
      `SELECT * FROM (
        SELECT id::text, state::text, data, output, retrycount, createdon, startedon, completedon, startafter
          FROM pgboss.job WHERE name = $1
        UNION ALL
        SELECT id::text, state::text, data, output, retrycount, createdon, startedon, completedon, startafter
          FROM pgboss.archive WHERE name = $1
      ) runs ORDER BY createdon DESC LIMIT 50`,
      req.params.name
    )
    res.json({ runs })
  } catch (error) {
    res.status(500).json({ message: getErrorMessage(error) })
  }
})

function getJob(name: string): any {
  const job = (jobs as Record<string, any>)[name]
  return job && typeof job.submit === 'function' ? job : null
}

function describeUser(user: any): RecordedRequest['user'] {
  if (!user || typeof user !== 'object') return null
  const identities = user.identities && typeof user.identities === 'object' ? user.identities : {}
  const identity = Object.entries(identities).find(([, value]) => value)
  return {
    id: String(user.id),
    identity: identity ? `${identity[0]}: ${(identity[1] as any).id}` : null,
  }
}

function flattenHeaders(headers: Record<string, unknown>): Record<string, string> {
  return Object.fromEntries(
    Object.entries(headers)
      .filter(([, value]) => value !== undefined)
      .map(([key, value]) => [key, Array.isArray(value) ? value.join(', ') : String(value)])
  )
}

function getErrorMessage(error: unknown): string {
  return error instanceof Error ? error.message : String(error)
}
