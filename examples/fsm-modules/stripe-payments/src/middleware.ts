import type { MiddlewareConfigFn } from "wasp/server";
import express from "express";

export const webhookMiddleware: MiddlewareConfigFn = (middlewareConfig) => {
  middlewareConfig.delete("express.json");
  middlewareConfig.set(
    "express.raw",
    express.raw({ type: "application/json" }),
  );
  return middlewareConfig;
};
