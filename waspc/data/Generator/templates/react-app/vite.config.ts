{{={= =}=}}
/// <reference types="vitest" />
import { mergeConfig } from "vite";
import { wasp } from "wasp/client/vite";

{=# customViteConfig.isDefined =}
// Ignoring the TS error because we are importing a file outside of TS root dir.
// @ts-ignore
{=& customViteConfig.importStatement =}
const _waspUserProvidedConfig = {=& customViteConfig.importIdentifier =}
{=/ customViteConfig.isDefined =}
{=^ customViteConfig.isDefined =}
const _waspUserProvidedConfig = {};
{=/ customViteConfig.isDefined =}

const defaultViteConfig = {
  plugins: [wasp()],
};

// https://vitejs.dev/config/
export default mergeConfig(
  defaultViteConfig,
  _waspUserProvidedConfig
);
