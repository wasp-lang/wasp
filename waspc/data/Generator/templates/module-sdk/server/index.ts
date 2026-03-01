export { HttpError } from './HttpError.js'
export type { MiddlewareConfigFn, MiddlewareConfig } from './middleware/globalMiddleware.js'

// Config type stub — at runtime the host app provides the real config
export declare const config: {
  readonly env: string
  readonly isDevelopment: boolean
  readonly port: number
  readonly databaseUrl: string
  readonly frontendUrl: string
  readonly serverUrl: string
  readonly allowedCORSOrigins: readonly (string | RegExp)[]
}
