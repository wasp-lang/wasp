import { type Plugin } from "vite";

export function waspBuildConfig(): Plugin {
  return {
    name: "wasp-build-config",
    config() {
      return {
        appType: "custom",
        build: {
          rollupOptions: {
            input: "virtual:wasp/index",
          },
        },
      };
    },
  };
}
