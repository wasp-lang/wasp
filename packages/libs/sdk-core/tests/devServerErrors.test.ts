import { mkdtemp, rm } from "node:fs/promises";
import type { AddressInfo } from "node:net";
import { tmpdir } from "node:os";
import path from "node:path";
import { createServer, type ViteDevServer } from "vite";
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";
import { sendServerErrorToDevClient } from "../src/devServerErrors/node";
import {
  SERVER_ERROR_CONSOLE_PREFIX,
  SERVER_ERROR_EVENT,
  SERVER_ERROR_PATH,
} from "../src/devServerErrors/shared";
import { serverErrors } from "../src/node/vite/plugins/serverErrors";

describe("server errors in the browser", () => {
  let root: string;
  let server: ViteDevServer;
  let clientUrl: string;

  beforeEach(async () => {
    root = await mkdtemp(path.join(tmpdir(), "wasp-server-errors-"));
    server = await createServer({
      root,
      configFile: false,
      logLevel: "silent",
      base: "/app/",
      plugins: [serverErrors()],
      server: { port: 0, forwardConsole: true },
    });
    await server.listen();
    const { port } = server.httpServer!.address() as AddressInfo;
    clientUrl = `http://localhost:${port}/app`;
  });

  afterEach(async () => {
    await server.close();
    await rm(root, { recursive: true, force: true });
  });

  it("pushes the errors the server sends to the browser", async () => {
    const browser = await connectBrowser(server);
    const event = browser.nextCustomEvent();

    await sendServerErrorToDevClient(clientUrl, "Something broke");

    await expect(event).resolves.toEqual({
      event: SERVER_ERROR_EVENT,
      data: { message: "Something broke" },
    });
    browser.close();
  });

  it("doesn't forward the browser's log of a server error to the terminal", async () => {
    const logError = vi.spyOn(server.config.logger, "error");
    const browser = await connectBrowser(server);

    browser.sendConsoleError(`${SERVER_ERROR_CONSOLE_PREFIX} Something broke`);
    browser.sendConsoleError("A browser error");

    await vi.waitFor(() => expect(logError).toHaveBeenCalled());
    expect(logError).toHaveBeenCalledOnce();
    expect(logError.mock.calls[0]![0]).toContain("A browser error");
    browser.close();
  });

  it.each([
    ["a non-JSON body", "text/plain", '{"message":"x"}', 403],
    ["an invalid payload", "application/json", '{"msg":"x"}', 400],
    ["malformed JSON", "application/json", "{", 400],
  ])("rejects %s", async (_name, contentType, body, expectedStatus) => {
    const response = await fetch(`${clientUrl}/${SERVER_ERROR_PATH}`, {
      method: "POST",
      headers: { "Content-Type": contentType },
      body,
    });
    expect(response.status).toBe(expectedStatus);
  });

  it("doesn't throw when the client dev server is unreachable", async () => {
    await server.close();
    await expect(
      sendServerErrorToDevClient(clientUrl, "Something broke"),
    ).resolves.toBeUndefined();
  });
});

async function connectBrowser(server: ViteDevServer) {
  const { port } = server.httpServer!.address() as AddressInfo;
  const socket = new WebSocket(
    `ws://localhost:${port}/app/?token=${server.config.webSocketToken}`,
    "vite-hmr",
  );
  await new Promise((resolve, reject) => {
    socket.addEventListener("open", resolve, { once: true });
    socket.addEventListener("error", reject, { once: true });
  });

  return {
    nextCustomEvent: () =>
      new Promise((resolve) => {
        socket.addEventListener("message", function onMessage({ data }) {
          const payload = JSON.parse(String(data));
          if (payload.type === "custom") {
            socket.removeEventListener("message", onMessage);
            resolve({ event: payload.event, data: payload.data });
          }
        });
      }),
    sendConsoleError: (message: string) =>
      socket.send(
        JSON.stringify({
          type: "custom",
          event: "vite:forward-console",
          data: { type: "log", data: { level: "error", message } },
        }),
      ),
    close: () => socket.close(),
  };
}
