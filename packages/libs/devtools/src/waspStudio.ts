import type {
  DevToolsChildProcessTerminalSession,
  DevToolsDockUserEntry,
} from "@vitejs/devtools-kit";
import { createProcessLauncher } from "@vitejs/devtools-kit/node";
import type { Plugin } from "vite";

import { waitForServer } from "./waitForServer";

const WASP_STUDIO_ID = "wasp-studio";

// `wasp studio` always serves on this port.
const WASP_STUDIO_URL = "http://localhost:4000/";

/**
 * Adds Wasp Studio as a panel to Vite DevTools. The panel runs `wasp studio`
 * when the user opens it, and embeds it once it's up.
 */
export function waspStudio(): Plugin {
  let projectRoot = process.cwd();
  let studioSession: DevToolsChildProcessTerminalSession | undefined;

  const launcher = createProcessLauncher({
    id: WASP_STUDIO_ID,
    title: "Wasp Studio",
    icon: "ph:graph-duotone",
    description: "See how the parts of your app connect.",
    buttonStart: "Start Wasp Studio",
    buttonLoading: "Starting Wasp Studio…",
    process: () => ({
      // `wasp start` passes the path to its own executable, so the panel uses
      // the same Wasp version as the app.
      command: process.env["WASP_EXE_PATH"] ?? "wasp",
      args: ["studio"],
      cwd: projectRoot,
    }),
    serve: {
      onReady: async (session) => {
        studioSession = session;
        await waitForServer(
          WASP_STUDIO_URL,
          () => session.status === "running",
        );
        return WASP_STUDIO_URL;
      },
    },
  });

  return {
    ...launcher,
    devtools: {
      ...launcher.devtools!,
      setup: (ctx) =>
        launcher.devtools!.setup(
          // The launcher doesn't take a category, so we add it to the dock
          // entries it registers. Listed first, with the framework's own tools.
          withDockCategory(ctx, "framework"),
        ),
    },
    apply: "serve",
    configResolved(config) {
      projectRoot = config.root;
    },
    configureServer(server) {
      // Vite DevTools leaves the process running when the dev server stops.
      server.httpServer?.once("close", () => {
        studioSession?.getChildProcess()?.kill();
      });
    },
  };
}

function withDockCategory<Context extends { docks: object }>(
  ctx: Context,
  category: string,
): Context {
  const withCategory = (entry: DevToolsDockUserEntry) => ({
    ...entry,
    category,
  });
  const docks = new Proxy(ctx.docks, {
    get(target, property) {
      const value: unknown = Reflect.get(target, property);
      if (typeof value !== "function") return value;
      if (property === "register" || property === "update") {
        return (entry: DevToolsDockUserEntry, ...rest: unknown[]) =>
          value.call(target, withCategory(entry), ...rest);
      }
      return value.bind(target);
    },
  });
  return new Proxy(ctx, {
    get: (target, property) =>
      property === "docks" ? docks : Reflect.get(target, property),
  });
}
