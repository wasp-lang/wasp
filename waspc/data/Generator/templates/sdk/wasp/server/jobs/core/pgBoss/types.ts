import type {
  JSONObject,
  JSONValue,
} from "../../../../core/serialization/index.js";
import { PrismaDelegate } from "../../../_types/index.js";
import type { JobFn as BaseJobFn } from "../../../types/base.js";

// PRIVATE API
export type JobFn<
  Input extends JSONObject,
  Output extends JSONValue | void,
  Entities extends Partial<PrismaDelegate>,
> = BaseJobFn<Input, Output, { entities: Entities }>;
