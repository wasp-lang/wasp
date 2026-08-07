import * as jobs from "wasp/server/jobs";
import type { ApiFn, JobFn } from "wasp/server/types";
import type {
  ModuleJobRequest,
  ModuleJobResponse,
  ModulePingResponse,
} from "./moduleApiContract";

type ApiRequest = {
  query: Partial<Record<keyof ModuleJobRequest, string>>;
};

type ApiResponse<Body> = {
  status(statusCode: number): ApiResponse<Body>;
  json(body: Body): void;
};

export const handleModulePing: ApiFn<
  unknown,
  ApiResponse<ModulePingResponse>
> = (_req, res) => {
  res.status(200).json({ ok: true });
};

export const startModuleJob: ApiFn<
  ApiRequest,
  ApiResponse<ModuleJobResponse>
> = async (req, res) => {
  const source = req.query.source;
  const requestedAt = req.query.requestedAt;
  if (
    (source !== "module-page" && source !== "host-page") ||
    requestedAt === undefined
  ) {
    throw new Error("Invalid module job request.");
  }

  const submittedJob = await jobs.moduleJob.submit({ source, requestedAt });
  res.status(202).json({ jobId: submittedJob.jobId });
};

export const moduleJob: JobFn<ModuleJobRequest, void> = async (args) => {
  console.log(
    `Full-stack module job requested by ${args.source} at ${args.requestedAt}.`,
  );
};
