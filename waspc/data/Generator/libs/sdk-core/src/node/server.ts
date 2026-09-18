import type { Application } from "express";
import type { Server } from "http";

export type ServerSetupFn = (context: ServerSetupFnContext) => Promise<void>;

// PRIVATE API (server)
export type ServerSetupFnContext = {
  app: Application;
  server: Server;
};
