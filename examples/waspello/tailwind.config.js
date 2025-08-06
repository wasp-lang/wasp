import { resolveProjectPath } from "wasp/dev";

/** @type {import('tailwindcss').Config} */
export default {
  content: [resolveProjectPath("./src/**/*.{js,jsx,ts,tsx}")],
  corePlugins: {
    //preflight: false
  },
  theme: {
    extend: {},
  },
  plugins: [],
};
