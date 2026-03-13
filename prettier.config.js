import prettierOrganizeImports from "prettier-plugin-organize-imports";
import prettierPkg from "prettier-plugin-pkg";
import * as prettierSh from "prettier-plugin-sh";
import * as prettierTailwindcss from "prettier-plugin-tailwindcss";

/**
 * @see https://prettier.io/docs/configuration
 * @type {import("prettier").Config}
 */
const config = {
  semi: true,
  singleQuote: false,
  jsxSingleQuote: false,
  trailingComma: "all",
  plugins: [
    prettierOrganizeImports,
    prettierPkg,
    prettierSh,
    prettierTailwindcss,
  ],
};

export default config;
