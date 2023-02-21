import express from 'express'
import http from 'http'

export type Application = express.Application

export type Server = http.Server

export type ServerSetupFn = (context: ServerSetupFnContext) => Promise<void>

export type ServerSetupFnContext = {
  app: Application,
  server: Server,
}
