export const appendToHead = (html: string, content: string) =>
  html.replace(
    /<\/\s*head\s*>/i,
    (headClosingTag) => `${content}${headClosingTag}`,
  );
