import tailwindcss from "@tailwindcss/vite";
import flowbiteReact from "flowbite-react/plugin/vite";
import { defineConfig } from "vite";

export default defineConfig({
  server: {
    open: true,
  },
  plugins: [tailwindcss(), flowbiteReact()],
});
