import { api, apiNamespace, page, route } from "@wasp.sh/spec";

import {
  defaultMiddlewareForStreamingText,
  streamingText,
} from "./api" with { type: "ref" };
import { StreamingTestPage } from "./pages/StreamingTestPage" with { type: "ref" };

export const streaming = [
  api("GET", "/api/streaming-test", streamingText),
  apiNamespace("/api/streaming-test", {
    middlewareConfigFn: defaultMiddlewareForStreamingText,
  }),
  route("StreamingRoute", "/streaming", page(StreamingTestPage)),
];
