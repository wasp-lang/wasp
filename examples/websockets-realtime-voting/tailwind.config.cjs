const { makeProjectGlobPattern } = require('wasp/client')

/** @type {import('tailwindcss').Config} */
module.exports = {
  content: [
    makeProjectGlobPattern("./src/**/*.{js,jsx,ts,tsx}"),
    makeProjectGlobPattern("node_modules/flowbite-react/**/*.{js,jsx,ts,tsx}"),
  ],
  theme: {
    extend: {},
  },
  plugins: [require("flowbite/plugin")],
};
