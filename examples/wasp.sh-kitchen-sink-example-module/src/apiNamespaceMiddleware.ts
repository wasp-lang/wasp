import type { MiddlewareConfigFn } from "wasp/server/types";
import {
  MODULE_API_HEADER_NAME,
  MODULE_API_HEADER_VALUE,
} from "./moduleApiContract";

type ModuleMiddlewareConfig = Map<string, any>;

type ModuleApiResponse = {
  setHeader(name: string, value: string): void;
};

export const moduleApiNamespaceMiddlewareFn: MiddlewareConfigFn<
  ModuleMiddlewareConfig
> = (config) => {
  config.set(
    "module.api",
    (_req: unknown, res: ModuleApiResponse, next: () => void) => {
      // Without exposing the header, the cross-origin dev client cannot read it.
      res.setHeader("Access-Control-Expose-Headers", MODULE_API_HEADER_NAME);
      res.setHeader(MODULE_API_HEADER_NAME, MODULE_API_HEADER_VALUE);
      next();
    },
  );
  return config;
};
