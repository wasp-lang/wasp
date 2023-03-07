import { Application } from 'express'
import { Server } from 'http'

export type ServerSetupFn = (context: ServerSetupFnContext) => Promise<void>

export type ServerSetupFnContext = {
  app: Application,
  server: Server,
}

export { Application, Request, Response } from 'express'
export { Server } from 'http'
