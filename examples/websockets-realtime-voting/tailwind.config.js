import FlowbitePlugin from "flowbite/plugin";
import { resolveProjectPath } from "wasp/dev";

/** @type {import('tailwindcss').Config} */
export default {
  content: [
    resolveProjectPath("./src/**/*.{js,jsx,ts,tsx}"),
    resolveProjectPath("node_modules/flowbite-react/**/*.{js,jsx,ts,tsx}"),
  ],
  theme: {
    extend: {},
  },
  plugins: [FlowbitePlugin],
};
