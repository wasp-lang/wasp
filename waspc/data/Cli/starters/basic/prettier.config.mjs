/**
 * @type {import("prettier").Config}
 */
const config = {
  plugins: ["prettier-plugin-tailwindcss"],
  // Tailwind v4 reads the theme from CSS, so the plugin needs the stylesheet
  // to sort the classes of the custom colors.
  tailwindStylesheet: "./src/App.css",
};

export default config;
