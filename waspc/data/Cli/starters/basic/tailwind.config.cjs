import { resolveProjectPath } from "wasp/dev";

/** @type {import('tailwindcss').Config} */
module.exports = {
  content: [resolveProjectPath("./src/**/*.{js,jsx,ts,tsx}")],
  theme: {
    extend: {
      fontSize: {
        "tiny": ["0.625rem", "1rem"], // 10px
      },
      colors: {
        // Created using https://www.tints.dev
        primary: {
          50: "#FFFBEB",
          100: "#FFF7D6",
          200: "#FFEFAD",
          300: "#FFE680",
          400: "#FFDA47",
          500: "#FFCC00",
          600: "#E6B800",
          700: "#CCA300",
          800: "#A88700",
          900: "#7A6200",
          950: "#574500"
        },
      },
    },
  },
  plugins: [],
};
