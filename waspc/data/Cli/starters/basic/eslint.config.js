import js from "@eslint/js";
import eslintPrettier from "eslint-config-prettier/flat";
import eslintReact from "eslint-plugin-react";
import { defineConfig } from "eslint/config";
import globals from "globals";
import eslintTypescript from "typescript-eslint";

export default defineConfig([
  { files: ["**/*.{js,mjs,cjs,ts,mts,cts,jsx,tsx}"], plugins: { js }, extends: ["js/recommended"] },
  { files: ["**/*.{js,mjs,cjs,ts,mts,cts,jsx,tsx}"], languageOptions: { globals: { ...globals.browser, ...globals.node } } },
  eslintTypescript.configs.recommended,
  eslintReact.configs.flat.recommended,
  eslintReact.configs.flat['jsx-runtime'],
  eslintPrettier,
  // Overrides:
  {
    // `@typescript-eslint/no-require-imports` is enabled by default in `typescript-eslint/recommended` config.
    // This allows us to use `require` syntax in CJS files.
    files: ["**/*.{cjs,cts}"],
    rules: {
      "@typescript-eslint/no-require-imports": ["off"]
    }
  }
]);
