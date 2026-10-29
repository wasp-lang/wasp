import { api, type Spec } from "@wasp/spec";

import {
  throwConcealedError,
  throwHttpError,
  throwRateLimitError,
  throwUnavailableError,
  throwUnexpectedError,
  throwUpstreamError,
} from "./apis" with { type: "ref" };

export const errorsSpec: Spec = [
  api("GET", "/errors/unexpected", throwUnexpectedError, { auth: false }),
  api("GET", "/errors/http", throwHttpError, { auth: false }),
  api("GET", "/errors/concealed", throwConcealedError, { auth: false }),
  api("GET", "/errors/unavailable", throwUnavailableError, { auth: false }),
  api("GET", "/errors/rate-limit", throwRateLimitError, { auth: false }),
  api("GET", "/errors/upstream", throwUpstreamError, { auth: false }),
];
