import * as path from "node:path";
import { DiagnosticCategory, type Diagnostic } from "typescript/unstable/sync";

// TypeScript 7's API returns diagnostics as plain data and has no formatter,
// so this reproduces the output of TypeScript 6's
// `formatDiagnosticsWithColorAndContext` and `formatDiagnostic`.

const NEW_LINE = "\n";

const Color = {
  Grey: "\u001b[90m",
  Red: "\u001b[91m",
  Yellow: "\u001b[93m",
  Blue: "\u001b[94m",
  Cyan: "\u001b[96m",
} as const;
const GUTTER_STYLE = "\u001b[7m";
const RESET = "\u001b[0m";
const ELLIPSIS = "...";
const HALF_INDENT = "  ";
const INDENT = "    ";

export type DiagnosticsFormatHost = {
  // Diagnostic file names are shown relative to this directory.
  currentDirectory: string;
  // The text diagnostic positions refer to. `undefined` if it can't be read.
  readFile: (fileName: string) => string | undefined;
};

export function formatDiagnosticsWithColorAndContext(
  diagnostics: readonly Diagnostic[],
  host: DiagnosticsFormatHost,
): string {
  const sourceFiles = new SourceFileCache(host);
  let output = "";

  for (const diagnostic of diagnostics) {
    const file = sourceFiles.get(diagnostic.fileName);
    if (file) {
      output += formatLocation(file, diagnostic.pos, host) + " - ";
    }
    output += colorize(
      categoryName(diagnostic.category),
      getCategoryColor(diagnostic.category),
    );
    output += colorize(` TS${diagnostic.code}: `, Color.Grey);
    output += flattenMessageText(diagnostic);

    if (file) {
      output += NEW_LINE;
      output += formatCodeSpan(
        file,
        diagnostic.pos,
        diagnostic.end,
        "",
        getCategoryColor(diagnostic.category),
      );
    }

    if (diagnostic.relatedInformation) {
      output += NEW_LINE;
      for (const related of diagnostic.relatedInformation) {
        const relatedFile = sourceFiles.get(related.fileName);
        if (relatedFile) {
          output += NEW_LINE;
          output +=
            HALF_INDENT + formatLocation(relatedFile, related.pos, host);
          output += formatCodeSpan(
            relatedFile,
            related.pos,
            related.end,
            INDENT,
            Color.Cyan,
          );
        }
        output += NEW_LINE;
        output += INDENT + flattenMessageText(related);
      }
    }

    output += NEW_LINE;
  }

  return output;
}

// The plain `file(line,col): error TSxxxx: message` format.
export function formatDiagnostics(
  diagnostics: readonly Diagnostic[],
  host: DiagnosticsFormatHost,
): string {
  const sourceFiles = new SourceFileCache(host);
  let output = "";

  for (const diagnostic of diagnostics) {
    const file = sourceFiles.get(diagnostic.fileName);
    if (file) {
      const { line, character } = file.getLineAndCharacter(diagnostic.pos);
      output += `${relativeFileName(file.fileName, host)}(${line + 1},${character + 1}): `;
    }
    output += `${categoryName(diagnostic.category)} TS${diagnostic.code}: ${flattenMessageText(diagnostic)}${NEW_LINE}`;
  }

  return output;
}

// The order and deduplication of TypeScript's `getPreEmitDiagnostics`. The
// API returns some diagnostics from more than one of its methods.
export function sortAndDeduplicateDiagnostics(
  diagnostics: readonly Diagnostic[],
): Diagnostic[] {
  const uniqueDiagnostics = new Map(
    diagnostics.map((diagnostic) => [JSON.stringify(diagnostic), diagnostic]),
  );
  return [...uniqueDiagnostics.values()].sort(compareDiagnostics);
}

function compareDiagnostics(a: Diagnostic, b: Diagnostic): number {
  return (
    compareOptionalStrings(a.fileName, b.fileName) ||
    a.pos - b.pos ||
    a.end - a.pos - (b.end - b.pos) ||
    a.code - b.code ||
    compareOptionalStrings(a.text, b.text)
  );
}

function compareOptionalStrings(
  a: string | undefined,
  b: string | undefined,
): number {
  if (a === b) return 0;
  if (a === undefined) return -1;
  if (b === undefined) return 1;
  return a < b ? -1 : 1;
}

// A diagnostic's message, followed by its chain of details, each level
// indented by two more spaces.
function flattenMessageText(diagnostic: Diagnostic, indentLevel = 0): string {
  let result = "";
  if (indentLevel > 0) {
    result += NEW_LINE + "  ".repeat(indentLevel);
  }
  result += diagnostic.text;
  for (const detail of diagnostic.messageChain ?? []) {
    result += flattenMessageText(detail, indentLevel + 1);
  }
  return result;
}

function formatLocation(
  file: SourceFile,
  position: number,
  host: DiagnosticsFormatHost,
): string {
  const { line, character } = file.getLineAndCharacter(position);
  return (
    colorize(relativeFileName(file.fileName, host), Color.Cyan) +
    ":" +
    colorize(`${line + 1}`, Color.Yellow) +
    ":" +
    colorize(`${character + 1}`, Color.Yellow)
  );
}

// The source lines the diagnostic spans, with `~` under the span. Spans longer
// than five lines show their first two and last two lines.
function formatCodeSpan(
  file: SourceFile,
  start: number,
  end: number,
  indent: string,
  squiggleColor: string,
): string {
  const { line: firstLine, character: firstLineChar } =
    file.getLineAndCharacter(start);
  const { line: lastLine, character: lastLineChar } =
    file.getLineAndCharacter(end);
  const lastLineInFile = file.lineStarts.length - 1;

  const hasMoreThanFiveLines = lastLine - firstLine >= 4;
  let gutterWidth = `${lastLine + 1}`.length;
  if (hasMoreThanFiveLines) {
    gutterWidth = Math.max(ELLIPSIS.length, gutterWidth);
  }

  let context = "";
  for (let i = firstLine; i <= lastLine; i++) {
    context += NEW_LINE;
    if (hasMoreThanFiveLines && firstLine + 1 < i && i < lastLine - 1) {
      context +=
        indent +
        colorize(ELLIPSIS.padStart(gutterWidth), GUTTER_STYLE) +
        " " +
        NEW_LINE;
      i = lastLine - 1;
    }

    const lineStart = file.lineStarts[i]!;
    const lineEnd =
      i < lastLineInFile ? file.lineStarts[i + 1]! : file.text.length;
    const lineContent = file.text
      .slice(lineStart, lineEnd)
      .trimEnd()
      .replace(/\t/g, " ");

    context +=
      indent + colorize(`${i + 1}`.padStart(gutterWidth), GUTTER_STYLE) + " ";
    context += lineContent + NEW_LINE;
    context += indent + colorize("".padStart(gutterWidth), GUTTER_STYLE) + " ";
    context += squiggleColor;
    if (i === firstLine) {
      const lastCharForLine = i === lastLine ? lastLineChar : undefined;
      context += lineContent.slice(0, firstLineChar).replace(/\S/g, " ");
      context += lineContent
        .slice(firstLineChar, lastCharForLine)
        .replace(/./g, "~");
    } else if (i === lastLine) {
      context += lineContent.slice(0, lastLineChar).replace(/./g, "~");
    } else {
      context += lineContent.replace(/./g, "~");
    }
    context += RESET;
  }
  return context;
}

function relativeFileName(
  fileName: string,
  host: DiagnosticsFormatHost,
): string {
  return path.isAbsolute(fileName)
    ? path.relative(host.currentDirectory, fileName)
    : fileName;
}

function categoryName(category: DiagnosticCategory): string {
  switch (category) {
    case DiagnosticCategory.Error:
      return "error";
    case DiagnosticCategory.Warning:
      return "warning";
    case DiagnosticCategory.Suggestion:
      return "suggestion";
    case DiagnosticCategory.Message:
      return "message";
  }
}

function getCategoryColor(category: DiagnosticCategory): string {
  switch (category) {
    case DiagnosticCategory.Error:
      return Color.Red;
    case DiagnosticCategory.Warning:
      return Color.Yellow;
    case DiagnosticCategory.Suggestion:
    case DiagnosticCategory.Message:
      return Color.Blue;
  }
}

function colorize(text: string, color: string): string {
  return color + text + RESET;
}

type SourceFile = {
  fileName: string;
  text: string;
  lineStarts: number[];
  getLineAndCharacter: (position: number) => {
    line: number;
    character: number;
  };
};

class SourceFileCache {
  private readonly files = new Map<string, SourceFile | undefined>();

  constructor(private readonly host: DiagnosticsFormatHost) {}

  get(fileName: string | undefined): SourceFile | undefined {
    if (fileName === undefined) return undefined;
    if (!this.files.has(fileName)) {
      const text = this.host.readFile(fileName);
      this.files.set(
        fileName,
        text === undefined ? undefined : createSourceFile(fileName, text),
      );
    }
    return this.files.get(fileName);
  }
}

// Positions are UTF-16 offsets, like JavaScript string indices.
function createSourceFile(fileName: string, text: string): SourceFile {
  const lineStarts = computeLineStarts(text);
  return {
    fileName,
    text,
    lineStarts,
    getLineAndCharacter(position) {
      let low = 0;
      let high = lineStarts.length - 1;
      while (low < high) {
        const middle = Math.ceil((low + high) / 2);
        if (lineStarts[middle]! <= position) low = middle;
        else high = middle - 1;
      }
      return { line: low, character: position - lineStarts[low]! };
    },
  };
}

// Line breaks as TypeScript counts them: `\r\n`, `\r`, `\n`, U+2028, U+2029.
function computeLineStarts(text: string): number[] {
  const lineStarts = [0];
  for (let i = 0; i < text.length; i++) {
    const char = text[i];
    if (char === "\r") {
      if (text[i + 1] === "\n") i++;
      lineStarts.push(i + 1);
    } else if (char === "\n" || char === " " || char === " ") {
      lineStarts.push(i + 1);
    }
  }
  return lineStarts;
}
