import { config } from "wasp/client";
import {
  getModuleJobApiPath,
  type ModuleJobResponse,
  type ModuleJobSource,
} from "./moduleJobContract";

export async function requestModuleJob(
  source: ModuleJobSource,
  modulePrefix: string = getCurrentPathname(),
): Promise<ModuleJobResponse> {
  const url = new URL(getModuleJobApiPath(modulePrefix), config.apiUrl);
  url.searchParams.set("source", source);
  url.searchParams.set("requestedAt", new Date().toISOString());

  const response = await fetch(url, { method: "POST" });
  if (!response.ok) {
    throw new Error(`Module job API returned ${response.status}.`);
  }

  return (await response.json()) as ModuleJobResponse;
}

function getCurrentPathname(): string {
  return (
    globalThis as unknown as {
      location: { pathname: string };
    }
  ).location.pathname;
}
