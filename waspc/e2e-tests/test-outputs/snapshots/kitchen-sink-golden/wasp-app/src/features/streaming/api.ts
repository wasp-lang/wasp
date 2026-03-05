import { type MiddlewareConfigFn } from "wasp/server";
import { type StreamingText } from "wasp/server/api";

// Custom API endpoint that returns a streaming text.
export const getText: StreamingText = async (_req, res, _context) => {
  res.setHeader("Content-Type", "text/html; charset=utf-8");
  res.setHeader("Transfer-Encoding", "chunked");

  let counter = 1;
  res.write("Hm, let me see...\n");
  while (counter <= 10) {
    // Send a chunk of data.
    if (counter === 10) {
      res.write(`and finally about ${counter}.`);
    } else {
      res.write(`let's talk about number ${counter} and `);
    }
    counter++;
    // Wait for 500ms before sending the next chunk.
    // This is to simulate a long-running process.
    await new Promise((resolve) => setTimeout(resolve, 500));
  }

  // End the response.
  res.end();
};

// Returning the default config.
export const getMiddlewareConfig: MiddlewareConfigFn = (config) => {
  return config;
};
