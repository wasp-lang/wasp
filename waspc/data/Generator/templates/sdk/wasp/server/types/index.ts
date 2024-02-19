import { type Application } from 'express'
import { Server } from 'http'

// PUBLIC API
export type ServerSetupFn = (context: ServerSetupFnContext) => Promise<void>

// PRIVATE API (server)
export type ServerSetupFnContext = {
  app: Application,
  server: Server,
}
