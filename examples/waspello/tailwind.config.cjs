const { resolveProjectPath } = require('wasp/client')

/** @type {import('tailwindcss').Config} */
module.exports = {
  content: [
    resolveProjectPath('./src/**/*.{js,jsx,ts,tsx}'),
  ],
  corePlugins: {
    //preflight: false
  },
  theme: {
    extend: {},
  },
  plugins: [],
}
