module.exports = (Prism) => {
  Prism.languages.wasp = {
    "prisma-closure": {
      pattern: /{=psl[\s\S]*?psl=}/,
      inside: {
        prisma: {
          pattern: /[\s\S]+/,
          inside: Prism.languages.prisma,
        },
      },
    },
    comment: {
      pattern: /\/\/.*|\/\*[\s\S]*?\*\//,
      greedy: true,
    },
    "json-closure": {
      pattern: /{=json[\s\S]*?json=}/,
      inside: {
        punctuation: /[{}[\],]/,
        property: {
          pattern: /(^|[^\\])"(?:\\.|[^\\"\r\n])*"(?=\s*:)/,
          lookbehind: true,
          greedy: true,
        },
        string: {
          pattern: /(^|[^\\])"(?:\\.|[^\\"\r\n])*"(?!\s*:)/,
          lookbehind: true,
          greedy: true,
        },
        number: /-?\b\d+(?:\.\d+)?(?:e[+-]?\d+)?\b/i,
        operator: /:/,
        boolean: /\b(?:false|true)\b/,
        null: {
          pattern: /\bnull\b/,
          alias: "keyword",
        },
      },
    },
    "js-import": {
      pattern: /import.*",?/,
      inside: Prism.languages.javascript,
    },
    string: {
      pattern: /"(?:\\.|[^\\"\r\n])*"/,
      greedy: true,
    },
    number: /-?\d+(?:\.\d+)?/,
    boolean: /\b(?:true|false)\b/,
    enum: {
      pattern:
        /\b(EmailAndPassword|PostgreSQL|SQLite|Simple|PgBoss|SMTP|SendGrid|Mailgun)\b/,
      alias: "constant",
    },
    // dict: {
    //   pattern: /{[\s\S]*?}/,
    //   inside: {
    //     punctuation: /[{}[\],]|(\.\.\.)/,
    //     key: {
    //       pattern: /[a-zA-Z]+(?=:)/,
    //       alias: "property",
    //     },
    //     value: {
    //       pattern: /[^,\s\]]+/,
    //       alias: "variable",
    //     },
    //   },
    // },
    "dict-key": {
      pattern: /[a-zA-Z]+(?=:)/,
      alias: "plain",
    },
    "storage-type": {
      pattern: /\b(action|apiNamespace|api|app|entity|job|page|query|route)\b/,
      alias: "keyword",
    },
    "class-name": {
      pattern: /[a-zA-Z][0-9a-zA-Z]*/,
      alias: "variable",
    },
    "http-method": {
      pattern: /\b(ALL|GET|POST|PUT|DELETE)\b/,
      alias: "constant",
    },
    array: {
      pattern: /\[[\s\S]*?\]/,
      inside: {
        punctuation: /[{}[\],]/,
        value: {
          pattern: /[^,\s\]]+/,
          alias: "variable",
        },
      },
    },
    punctuation: /[{}[\],]/,
  };
};
