const { resolveProjectPath } = require('wasp/server')
const { nextui } = require("@nextui-org/react");

/** @type {import('tailwindcss').Config} */
module.exports = {
  content: [
    resolveProjectPath("./src/**/*.{js,jsx,ts,tsx}"),
    resolveProjectPath("./node_modules/@nextui-org/theme/dist/**/*.{js,ts,jsx,tsx}"),
  ],
  theme: {
    extend: {},
  },
  darkMode: "class",
  plugins: [nextui()],
};
