import * as prettier from "prettier";

/**
 * Gets the Prettier configuration for the current directory
 */
async function getPrettierConfig() {
  const rawConfig = await prettier.resolveConfig(process.cwd(), {
    useCache: true,
    editorconfig: true,
  });

  const config = {
    ...rawConfig,
    // We don't need any non-default plugins for the case of simple JS/TS examples
    plugins: [],
  };

  return config;
}

// We will cache the Prettier configuration to avoid resolving it multiple times
let prettierConfig: prettier.Options | undefined;

export interface FormatCodeOptions {
  parser: prettier.Options["parser"];
}

export async function formatCode(
  code: string,
  { parser }: FormatCodeOptions,
): Promise<string> {
  prettierConfig ??= await getPrettierConfig();
  const formatted = await prettier.format(code, { ...prettierConfig, parser });
  return formatted.trim(); // prettier adds a trailing newline, we remove it
}
