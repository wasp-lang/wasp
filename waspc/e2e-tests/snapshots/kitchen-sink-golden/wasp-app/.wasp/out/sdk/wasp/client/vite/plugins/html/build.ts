import type { Plugin } from "vite";
import path from "node:path";
import { getIndexHtmlContent } from "../../virtual-files/index.js";

const indexHtmlFileName = "index.html";

const htmlVirtualModule = {
  id: indexHtmlFileName,
  absPath: "",
  getContent: () => getIndexHtmlContent(),
};

export function waspHtmlBuild(): Plugin {
  return {
    name: "wasp:html-build",
    config() {
      return {
        build: {
          rollupOptions: {
            // Vite tries to find the entry file on disk (which doesn't exist)
            // so the build fails. We tell Vite/Rollup to use `index.html` even
            // though it doesn't exist on the disk.
            input: indexHtmlFileName,
          },
        },
      };
    },
    configResolved(config) {
      htmlVirtualModule.absPath = path.resolve(config.root, htmlVirtualModule.id);
    },
    resolveId(id) {
      // Resolve index.html to a virtual module
      if (id === htmlVirtualModule.id) {
        return htmlVirtualModule.absPath;
      }
    },
    load(id) {
      // Provide content for virtual index.html
      if (id === htmlVirtualModule.absPath) {
        return htmlVirtualModule.getContent();
      }
    },
  };
}
