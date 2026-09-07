import { type RequestHandler } from "express";
import {
  type MiddlewareConfig as BaseMiddlewareConfig,
  type MiddlewareConfigFn as BaseMiddlewareConfigFn,
} from "../types/base.js";

// PUBLIC API
export type MiddlewareConfigFn = BaseMiddlewareConfigFn<MiddlewareConfig>;

// PRIVATE API
export type MiddlewareConfig = BaseMiddlewareConfig<RequestHandler>;
