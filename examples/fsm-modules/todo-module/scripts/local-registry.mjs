#!/usr/bin/env node
/**
 * Starts a local Verdaccio registry, publishes this module (and its
 * dependency wasp-config) to it, then keeps running so you can
 * `npm install` from it in the consuming app.
 *
 * Usage:  npm run local-registry          # start + publish
 *         npm run local-registry -- stop   # kill a running instance
 *
 * Prerequisites: npm install -g verdaccio@6 npm-cli-login@1
 */
import { execSync, spawn } from "node:child_process";
import { mkdirSync, writeFileSync, existsSync, readFileSync } from "node:fs";
import { homedir, tmpdir } from "node:os";
import { join, resolve } from "node:path";

const REGISTRY_URL = "http://localhost:4873";
const PID_FILE = join(tmpdir(), "verdaccio-todo-module.pid");
const VERDACCIO_CONFIG_DIR = join(homedir(), ".config", "verdaccio");
const VERDACCIO_CONFIG_FILE = join(VERDACCIO_CONFIG_DIR, "config.yaml");
const VERDACCIO_STORAGE = join(tmpdir(), "verdaccio-todo-module-storage");

const MODULE_DIR = resolve(import.meta.dirname, "..");
const WASP_CONFIG_DIR = resolve(MODULE_DIR, "../../../../waspc/packages/wasp-config");

// ── stop ────────────────────────────────────────────────────────────────
if (process.argv.includes("stop")) {
  if (existsSync(PID_FILE)) {
    const pid = readFileSync(PID_FILE, "utf8").trim();
    try {
      process.kill(Number(pid));
      console.log(`Stopped verdaccio (pid ${pid})`);
    } catch {
      console.log("Process already gone");
    }
  } else {
    console.log("No pid file found, nothing to stop");
  }
  process.exit(0);
}

// ── helpers ─────────────────────────────────────────────────────────────
function run(cmd, opts = {}) {
  console.log(`$ ${cmd}`);
  return execSync(cmd, { stdio: "inherit", ...opts });
}

function waitForRegistry(maxSeconds = 15) {
  const deadline = Date.now() + maxSeconds * 1000;
  while (Date.now() < deadline) {
    try {
      execSync(`curl -sSf ${REGISTRY_URL}/-/ping`, { stdio: "ignore" });
      return;
    } catch {
      execSync("sleep 0.5");
    }
  }
  throw new Error(`Verdaccio not ready after ${maxSeconds}s`);
}

// ── write verdaccio config ──────────────────────────────────────────────
mkdirSync(VERDACCIO_CONFIG_DIR, { recursive: true });
writeFileSync(
  VERDACCIO_CONFIG_FILE,
  `storage: ${VERDACCIO_STORAGE}
log: { type: stdout, format: pretty, level: warn }
max_body_size: 1000mb
uplinks:
  npmjs:
    url: https://registry.npmjs.org/
packages:
  "@waspello/*":
    access: $all
    publish: $all
  "wasp-config":
    access: $all
    publish: $all
  "**":
    access: $all
    publish: $all
    proxy: npmjs
`
);

// ── clean previous storage ──────────────────────────────────────────────
run(`rm -rf ${VERDACCIO_STORAGE}`);

// ── start verdaccio ─────────────────────────────────────────────────────
console.log("\nStarting verdaccio...");
const child = spawn("verdaccio", [], {
  stdio: "ignore",
  detached: true,
});
child.unref();
writeFileSync(PID_FILE, String(child.pid));
console.log(`Verdaccio pid: ${child.pid} (saved to ${PID_FILE})`);

waitForRegistry();
console.log("Verdaccio is ready\n");

// ── login ───────────────────────────────────────────────────────────────
run(
  `npm-cli-login -u fake -p fake -e fake@test.com -r ${REGISTRY_URL}`
);

// ── publish wasp-config ─────────────────────────────────────────────────
console.log("\nPublishing wasp-config...");
run(`npm publish --registry ${REGISTRY_URL}`, { cwd: WASP_CONFIG_DIR });

// ── publish todo-module ─────────────────────────────────────────────────
console.log("\nPublishing @waspello/todo-module...");
run(`npm publish --registry ${REGISTRY_URL}`, { cwd: MODULE_DIR });

// ── done ────────────────────────────────────────────────────────────────
const pkg = JSON.parse(readFileSync(join(MODULE_DIR, "package.json"), "utf8"));
console.log(`
Done! Registry is running at ${REGISTRY_URL}

To install in waspello:
  cd examples/waspello
  npm install @waspello/todo-module@${pkg.version} --registry ${REGISTRY_URL}

To stop the registry:
  npm run local-registry -- stop
`);
