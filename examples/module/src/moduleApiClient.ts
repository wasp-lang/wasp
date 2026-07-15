import { config } from "wasp/client";
import {
  getModuleJobApiPath,
  getModulePingApiPath,
  MODULE_API_HEADER_NAME,
  type ModuleJobResponse,
  type ModuleJobSource,
} from "./moduleApiContract";

export type ModulePingResult = {
  moduleApiHeader: string | null;
};

export async function pingModuleApi(
  modulePrefix: string = getCurrentPathname(),
): Promise<ModulePingResult> {
  const url = new URL(getModulePingApiPath(modulePrefix), config.apiUrl);

  const response = await fetch(url);
  if (!response.ok) {
    throw new Error(`Module ping API returned ${response.status}.`);
  }

  return {
    moduleApiHeader: response.headers.get(MODULE_API_HEADER_NAME),
  };
}

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

export function getCurrentPathname(): string {
  return (
    globalThis as unknown as {
      location: { pathname: string };
    }
  ).location.pathname;
}
