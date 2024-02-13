const { resolveProjectPath } = require('wasp/server')

/** @type {import('tailwindcss').Config} */
module.exports = {
  content: [resolveProjectPath('./src/**/*.{js,jsx,ts,tsx}')],
  theme: {
    extend: {},
  },
  plugins: [],
}
