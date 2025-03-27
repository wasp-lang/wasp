const { resolveProjectPath } = require("wasp/dev");
const { heroui } = require("@heroui/react");

/** @type {import('tailwindcss').Config} */
module.exports = {
  content: [
    resolveProjectPath("./src/**/*.{js,jsx,ts,tsx}"),
    resolveProjectPath(
      "./node_modules/@heroui/theme/dist/**/*.{js,ts,jsx,tsx}"
    ),
  ],
  theme: {
    extend: {},
  },
  darkMode: "class",
  plugins: [heroui()],
};
