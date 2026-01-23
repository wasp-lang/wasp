/// <reference types="vitest" />
import { mergeConfig } from "vite";
import { wasp } from "wasp/client/vite";

// Ignoring the TS error because we are importing a file outside of TS root dir.
// @ts-ignore
import customViteConfig from '../../../vite.config'
const _waspUserProvidedConfig = customViteConfig

const defaultViteConfig = {
  plugins: [wasp()],
};

// https://vitejs.dev/config/
export default mergeConfig(
  defaultViteConfig,
  _waspUserProvidedConfig
);
