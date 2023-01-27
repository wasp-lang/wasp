module.exports = {
  "env": {
    "es2020": true,
    "node": true
  },
  "root": true,
  "extends": [
    "eslint:recommended",
    "plugin:@typescript-eslint/recommended"
  ],
  "parser": "@typescript-eslint/parser",
  "parserOptions": {
    "ecmaVersion": 11,
    "sourceType": "module"
  },
  "plugins": [
    "@typescript-eslint"
  ],
  "rules": {
    "indent": [
      "error",
      "tab"
    ],
    "linebreak-style": [
      "error",
      "unix"
    ],
    "quotes": [
      "error",
      "single"
    ],
    "eol-last": [
      "error",
      "always"
    ],
    "no-multiple-empty-lines": [
      "error",
      {
        "max": 2,
        "maxEOF": 1
      }
    ],
    "comma-spacing": [
      "error",
      { "before": false, "after": true }
    ],
    "space-before-function-paren": ["error", {
      "anonymous": "always",
      "named": "never",
      "asyncArrow": "always"
    }],
    "comma-dangle": [
      "error",
      "always-multiline"
    ],
    "object-curly-spacing": [
      "error",
      "always"
    ],
    "padding-line-between-statements": [
      "error",
      { "blankLine": "always", "prev": "function", "next": "function" },
      { "blankLine": "always", "prev": "function", "next": "export" },
      { "blankLine": "always", "prev": "export", "next": "function" },
      { "blankLine": "always", "prev": "export", "next": "export" }
    ],
    "no-duplicate-imports": "error",
    "@typescript-eslint/semi": [
      "error",
      "always"
    ],
    "@typescript-eslint/member-delimiter-style": ["error", {
      "multiline": {
        "delimiter": "semi",
        "requireLast": true
      }
    }
    ]
  }
}
