export type ModuleJobSource = "module-page" | "host-page";

export type ModuleJobRequest = {
  source: ModuleJobSource;
  requestedAt: string;
};

export type ModuleJobResponse = {
  jobId: string;
};

export function getModuleJobApiPath(prefix: string): string {
  return `${prefix.replace(/\/$/, "")}/api/job`;
}
