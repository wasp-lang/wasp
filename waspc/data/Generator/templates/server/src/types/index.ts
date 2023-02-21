import express from 'express'
import http from 'http'

export type ServerSetupFn = (context: ServerSetupFnContext) => Promise<void>

export type ServerSetupFnContext = {
  app: express.Application,
  server: http.Server,
}
