import { commandSucceeds } from "./process.js";

export async function checkDependencies({
  signal,
}: {
  signal: AbortSignal;
}): Promise<void> {
  // ENOENT (command not found) surfaces as a spawn failure, i.e. `false`.
  const dockerAvailable = await commandSucceeds({
    name: "check-docker",
    cmd: "docker",
    args: ["--version"],
    signal,
  });

  if (!dockerAvailable) {
    throw new Error("Required command 'docker' not found. Please install it.");
  }
}
