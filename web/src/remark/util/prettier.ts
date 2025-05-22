import * as prettier from 'prettier'

let prettierConfigPromise: Promise<prettier.Options> | undefined

/**
 * Gets the Prettier configuration for the current directory. (memoized)
 */
export function getPrettierConfig() {
  prettierConfigPromise ??= prettier.resolveConfig(__dirname, {
    useCache: true,
    editorconfig: true,
  })
  return prettierConfigPromise
}

export async function formatCode(
  code: string,
  { parser }: { parser: prettier.Options['parser'] }
) {
  const prettierConfig = await getPrettierConfig()
  const formatted = await prettier.format(code, { ...prettierConfig, parser })
  return formatted.trim() // prettier adds a trailing newline, we remove it
}
