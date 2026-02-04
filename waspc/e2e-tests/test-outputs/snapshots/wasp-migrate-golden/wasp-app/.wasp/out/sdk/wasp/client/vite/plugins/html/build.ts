import type { Plugin } from "vite";
import { getIndexHtmlContent } from "../../virtual-files/index.js";
import { makeVirtualFilesResolver, type VirtualFiles } from "../../virtual-files/resolver.js";

const INDEX_HTML_FILE_NAME = "index.html";

const resolveVirtualFiles = makeVirtualFilesResolver([
  { id: INDEX_HTML_FILE_NAME, load: getIndexHtmlContent },
]);

export function waspHtmlBuild(): Plugin {
  let virtualFiles!: VirtualFiles;

  return {
    name: "wasp:html-build",
    apply: "build",
    config() {
      return {
        build: {
          rollupOptions: {
            // Vite tries to find the entry file on disk (which doesn't exist)
            // so the build fails. We tell Vite/Rollup to use `index.html` even
            // though it doesn't exist on the disk.
            input: INDEX_HTML_FILE_NAME,
          },
        },
      };
    },
    configResolved(config) {
      virtualFiles = resolveVirtualFiles(config.root);
    },
    resolveId: (id) => virtualFiles.ids.get(id),
    load(id) {
      const loader = virtualFiles.loaders.get(id);
      return loader?.();
    },
  };
}
