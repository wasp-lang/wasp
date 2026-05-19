import pluginJs from "@eslint/js";
import globals from "globals";
import tseslint from "typescript-eslint";

export default [
  pluginJs.configs.recommended,
  ...tseslint.configs.strict,
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
      "@typescript-eslint/no-unused-vars": [
        "error",
        {
          args: "all",
          argsIgnorePattern: "^_",
          caughtErrors: "all",
          caughtErrorsIgnorePattern: "^_",
          destructuredArrayIgnorePattern: "^_",
          varsIgnorePattern: "^_",
          ignoreRestSiblings: true,
        },
      ],
      "@typescript-eslint/no-empty-function": "warn",
      "no-empty": "warn",
      "no-constant-condition": "warn",
      "object-shorthand": "warn",
    },
  },
];
