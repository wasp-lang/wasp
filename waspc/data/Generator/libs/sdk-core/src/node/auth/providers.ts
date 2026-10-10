import type { Request, Router } from "express";
import type { ProviderName } from "../../auth/providerData.js";
export type ProviderConfig = {
  // Unique provider identifier, used as part of URL paths
  id: ProviderName;
  displayName: string;
  // Every provider must have a setupRouter method which returns the Express router.
  // In this function we are flexibile to do what ever is necessary to make the provider work.
  createRouter(provider: ProviderConfig): Router;
};

export type RequestWithWasp = Request & { wasp?: { [key: string]: any } };
