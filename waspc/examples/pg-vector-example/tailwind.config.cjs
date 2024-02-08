const { makeProjectGlobPattern } = require('wasp/client')
const { nextui } = require("@nextui-org/react");

/** @type {import('tailwindcss').Config} */
module.exports = {
  content: [
    makeProjectGlobPattern("./src/**/*.{js,jsx,ts,tsx}"),
    makeProjectGlobPattern("./node_modules/@nextui-org/theme/dist/**/*.{js,ts,jsx,tsx}"),
  ],
  theme: {
    extend: {},
  },
  darkMode: "class",
  plugins: [nextui()],
};
