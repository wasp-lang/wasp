import { api, page, query, route, type Spec } from "@wasp.sh/spec";

import {
  throwConcealedError,
  throwHttpError,
  throwRateLimitError,
  throwUnavailableError,
  throwUnexpectedError,
} from "./apis" with { type: "ref" };
import { ErrorsPage } from "./pages/ErrorsPage" with { type: "ref" };
import { getTeapot } from "./queries" with { type: "ref" };

export const errorsSpec: Spec = [
  api("GET", "/errors/unexpected", throwUnexpectedError, { auth: false }),
  api("GET", "/errors/http", throwHttpError, { auth: false }),
  api("GET", "/errors/concealed", throwConcealedError, { auth: false }),
  api("GET", "/errors/unavailable", throwUnavailableError, { auth: false }),
  api("GET", "/errors/rate-limit", throwRateLimitError, { auth: false }),
  query(getTeapot, { auth: false }),
  route("ErrorsRoute", "/errors", page(ErrorsPage)),
];
