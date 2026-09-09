import { api, apiNamespace, page, route, type Spec } from "@wasp.sh/spec";

import {
  defaultMiddlewareForStreamingText,
  streamingText,
} from "./api" with { type: "ref" };
import { StreamingTestPage } from "./pages/StreamingTestPage" with { type: "ref" };

export const streamingSpec: Spec = [
  api("GET", "/streaming-test", streamingText),
  apiNamespace("/streaming-test", {
    middlewareConfigFn: defaultMiddlewareForStreamingText,
  }),
  route("StreamingRoute", "/streaming", page(StreamingTestPage)),
];
