import { type Config } from "prettier";

const config: Config = {
  plugins: [
    "prettier-plugin-organize-imports",
    "prettier-plugin-pkg",
    "prettier-plugin-sh",
    "prettier-plugin-tailwindcss",
  ],
  semi: true,
  singleQuote: false,
  jsxSingleQuote: false,
  trailingComma: "all",
};

export default config;
