import { api, type Spec } from "@wasp.sh/spec";

import {
  throwHttpError,
  throwUnexpectedError,
} from "./apis" with { type: "ref" };

export const errorsSpec: Spec = [
  api("GET", "/errors/unexpected", throwUnexpectedError, { auth: false }),
  api("GET", "/errors/http", throwHttpError, { auth: false }),
];
