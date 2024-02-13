const { resolveProjectPath } = require('wasp/dev')

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
