import { heroui } from "@heroui/react";
import { resolveProjectPath } from "wasp/dev";

/** @type {import('tailwindcss').Config} */
export default {
  content: [
    resolveProjectPath("./src/**/*.{js,jsx,ts,tsx}"),
    resolveProjectPath(
      "./node_modules/@heroui/theme/dist/**/*.{js,ts,jsx,tsx}",
    ),
  ],
  theme: {
    extend: {},
  },
  darkMode: "class",
  plugins: [heroui()],
};
