import { resolveProjectPath } from "wasp/dev";

/** @type {import('tailwindcss').Config} */
export default {
  content: [resolveProjectPath("./src/**/*.{js,jsx,ts,tsx}")],
  theme: {
    extend: {
      colors: {
        primary: {
          50: "#fffee7",
          100: "#fffec1",
          200: "#fff886",
          300: "#ffec41",
          400: "#ffdb0d",
          500: "#ffcc00",
          600: "#d19500",
          700: "#a66a02",
          800: "#89530a",
          900: "#74430f",
          950: "#442304",
        },
      },
    },
  },
  plugins: [require("@tailwindcss/forms"), require("@tailwindcss/typography")],
};
