import * as jobs from "wasp/server/jobs";
import type { ModuleJobRequest, ModuleJobResponse } from "./moduleJobContract";

type ApiRequest = {
  query: Partial<Record<keyof ModuleJobRequest, string>>;
};

type ApiResponse<Body> = {
  status(statusCode: number): ApiResponse<Body>;
  json(body: Body): void;
};

export async function startModuleJob(
  req: ApiRequest,
  res: ApiResponse<ModuleJobResponse>,
  _context: unknown,
): Promise<void> {
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
}

export async function moduleJob(args: ModuleJobRequest): Promise<void> {
  console.log(
    `Full-stack module job requested by ${args.source} at ${args.requestedAt}.`,
  );
}
