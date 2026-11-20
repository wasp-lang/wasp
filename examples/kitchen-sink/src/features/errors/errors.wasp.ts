import { api, page, query, route, type Spec } from "@wasp/spec";

import {
  throwHttpError,
  throwUnexpectedError,
} from "./apis" with { type: "ref" };
import { ClientErrorsPage } from "./pages/ClientErrorsPage" with { type: "ref" };
import { ErrorsPage } from "./pages/ErrorsPage" with { type: "ref" };
import { getTeapot } from "./queries" with { type: "ref" };
import { getReportedServerErrors } from "./serverOnError" with { type: "ref" };

export const errorsSpec: Spec = [
  api("GET", "/errors/unexpected", throwUnexpectedError, { auth: false }),
  api("GET", "/errors/http", throwHttpError, { auth: false }),
  api("GET", "/errors/reported", getReportedServerErrors, { auth: false }),
  query(getTeapot, { auth: false }),
  route("ErrorsRoute", "/errors", page(ErrorsPage)),
  route("ClientErrorsRoute", "/errors/client", page(ClientErrorsPage)),
];
