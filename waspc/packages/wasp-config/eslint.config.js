import globals from "globals";
import pluginJs from "@eslint/js";
import tseslint from "typescript-eslint";

export default [
  pluginJs.configs.recommended,
  ...tseslint.configs.strict,
  // Todo: explore typed-linting: https://typescript-eslint.io/getting-started/typed-linting
  {
    languageOptions: {
      globals: globals.node,
    },
  },
  // global ignore
  {
    ignores: ["node_modules/", "dist/"],
  },
  {
    rules: {
      "@typescript-eslint/no-unused-vars": "warn",
      "@typescript-eslint/no-empty-function": "warn",
      "no-empty": "warn",
      "no-constant-condition": "warn",
    },
  },
];
