import js from "@eslint/js";
import reactPlugin from "eslint-plugin-react";
import reactHooksPlugin from "eslint-plugin-react-hooks";
import tseslint from "typescript-eslint";

export default tseslint.config(
  // Ignore patterns
  {
    ignores: [
      "**/node_modules/**",
      "**/.wasp/**",
      "**/dist/**",
      "**/build/**",
      "app_diff/**",
      "**/public/**/*.js",
      "**/.astro/**",
      "opensaas-sh/blog/scripts/**",
    ],
  },

  // Base JavaScript rules for all files
  js.configs.recommended,

  // TypeScript rules for TS/TSX files
  ...tseslint.configs.recommended,

  // React-specific rules
  {
    files: ["**/*.jsx", "**/*.tsx"],
    plugins: {
      react: reactPlugin,
      "react-hooks": reactHooksPlugin,
    },
    settings: {
      react: {
        version: "detect",
      },
    },
    rules: {
      ...reactPlugin.configs.recommended.rules,
      ...reactHooksPlugin.configs.recommended.rules,
      "react/react-in-jsx-scope": "off", // Not needed in React 17+
      "react/prop-types": "warn", // Using TypeScript for type checking
      "react/no-unescaped-entities": "off", // Allow apostrophes in JSX
    },
  },

  // CommonJS configuration files (postcss.config.cjs, etc.)
  {
    files: ["**/*.cjs"],
    languageOptions: {
      sourceType: "commonjs",
      globals: {
        module: "readonly",
        require: "readonly",
        __dirname: "readonly",
        __filename: "readonly",
        process: "readonly",
        console: "readonly",
      },
    },
  },

  // Node.js scripts and MJS files
  {
    files: ["**/*.mjs", "**/scripts/**/*.js"],
    languageOptions: {
      globals: {
        process: "readonly",
        console: "readonly",
        __dirname: "readonly",
        __filename: "readonly",
      },
    },
  },
);
