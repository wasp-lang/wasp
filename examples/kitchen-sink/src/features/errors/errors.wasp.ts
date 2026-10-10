import { api, type Spec } from "@wasp.sh/spec";

import {
  throwConcealedError,
  throwHttpError,
  throwRateLimitError,
  throwUnavailableError,
  throwUnexpectedError,
} from "./apis" with { type: "ref" };

export const errorsSpec: Spec = [
  api("GET", "/errors/unexpected", throwUnexpectedError, { auth: false }),
  api("GET", "/errors/http", throwHttpError, { auth: false }),
  api("GET", "/errors/concealed", throwConcealedError, { auth: false }),
  api("GET", "/errors/unavailable", throwUnavailableError, { auth: false }),
  api("GET", "/errors/rate-limit", throwRateLimitError, { auth: false }),
];
