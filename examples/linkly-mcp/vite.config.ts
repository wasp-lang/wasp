import tailwindcss from "@tailwindcss/vite";
import { defineConfig } from "vite";
import { wasp } from "wasp/client/vite";

export default defineConfig({
  plugins: [wasp(), tailwindcss()],
  server: {
    open: false,
  },
  // Pre-bundle dependencies that are only discovered on some pages, so Vite
  // doesn't reload the page the first time one of those pages is visited.
  optimizeDeps: {
    include: [
      "@hookform/resolvers/zod",
      "react-hook-form",
      "recharts",
      "radix-ui",
      "lucide-react",
      "sonner",
      "next-themes",
      "zod",
      "class-variance-authority",
      "cn",
      "react-markdown",
    ],
  },
});
