import type { IncomingMessage, ServerResponse } from 'node:http'
import { Readable } from 'node:stream'

/**
 * The Express boundary, in both directions: Wasp speaks Node HTTP to
 * Express and standard `Request` / `Response` to auth handlers, and converts
 * here, once.
 */

// PRIVATE API
/**
 * A standard `Request` built from a Node request. By default headers and URL
 * only, which is all authentication reads; with `body`, the raw request
 * stream rides along untouched, for a handler's own routes to parse.
 */
export function toWebRequest(
  req: IncomingMessage & { protocol?: string; originalUrl?: string },
  options: { body?: boolean } = {},
): Request {
  const headers = new Headers()
  for (const [key, value] of Object.entries(req.headers)) {
    if (typeof value === 'string') {
      headers.set(key, value)
    } else if (Array.isArray(value)) {
      headers.set(key, value.join(', '))
    }
  }
  const host = req.headers.host ?? 'localhost'
  const protocol = req.protocol ?? 'http'
  const method = req.method ?? 'GET'
  const hasBody = options.body === true && method !== 'GET' && method !== 'HEAD'
  return new Request(`${protocol}://${host}${req.originalUrl ?? req.url ?? '/'}`, {
    method,
    headers,
    ...(hasBody ? { body: Readable.toWeb(req) as ReadableStream, duplex: 'half' } : {}),
  } as RequestInit)
}

// PRIVATE API
/** Writes a standard `Response` to the Node response. */
export async function sendWebResponse(res: ServerResponse, response: Response): Promise<void> {
  res.statusCode = response.status
  response.headers.forEach((value, name) => {
    if (name !== 'set-cookie') {
      res.setHeader(name, value)
    }
  })
  const cookies = response.headers.getSetCookie()
  if (cookies.length > 0) {
    res.setHeader('set-cookie', cookies)
  }
  if (response.body === null) {
    res.end()
    return
  }
  res.end(Buffer.from(await response.arrayBuffer()))
}

// PRIVATE API
/** A JSON answer, the shape every default answer of Wasp's auth takes. */
export function jsonResponse(status: number, body: unknown, headers?: HeadersInit): Response {
  return Response.json(body, { status, headers })
}
