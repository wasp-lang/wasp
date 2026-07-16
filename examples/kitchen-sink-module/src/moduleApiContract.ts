export type ModuleJobSource = "module-page" | "host-page";

export type ModuleJobRequest = {
  source: ModuleJobSource;
  requestedAt: string;
};

export type ModuleJobResponse = {
  jobId: string;
};

export type ModulePingResponse = {
  ok: true;
};

export const MODULE_API_HEADER_NAME = "X-Module-Api";
export const MODULE_API_HEADER_VALUE = "@kitchen-sink/module";

export function getModuleApiPrefix(prefix: string): string {
  return `${prefix.replace(/\/$/, "")}/api`;
}

export function getModulePingApiPath(prefix: string): string {
  return `${getModuleApiPrefix(prefix)}/ping`;
}

export function getModuleJobApiPath(prefix: string): string {
  return `${getModuleApiPrefix(prefix)}/job`;
}
