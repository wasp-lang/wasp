import React from "react";

/**
 * ANSI color names, mapped to theme-aware colors in `custom.css`
 * (`.terminal__ansi-*`). `prompt` is for the shell prompt and the command the
 * user typed, which are not part of the program's output.
 */
export type TerminalColor =
  | "default"
  | "prompt"
  | "red"
  | "green"
  | "yellow"
  | "blue"
  | "magenta"
  | "cyan";

export type TerminalSegment = [TerminalColor, string];

/** One line of output, as a list of colored segments. An empty list is a blank line. */
export type TerminalLine = TerminalSegment[];

interface TerminalProps {
  /** Label shown in the window's title bar */
  title: string;
  lines: TerminalLine[];
}

/**
 * Renders terminal output as real, selectable text in a window-like frame that
 * follows the site's light/dark theme, with colors matching the ANSI colors of
 * the actual output.
 */
export function Terminal({ title, lines }: TerminalProps) {
  return (
    <div className="terminal">
      <div className="terminal__header">
        <div className="terminal__dots">
          <span />
          <span />
          <span />
        </div>
        <span className="terminal__title">{title}</span>
        {/* Spacer to keep the title centered */}
        <div className="terminal__dots" style={{ visibility: "hidden" }}>
          <span />
          <span />
          <span />
        </div>
      </div>
      <pre className="terminal__body">
        {lines.map((segments, i) => (
          <React.Fragment key={i}>
            {segments.map(([color, text], j) => (
              <span key={j} className={`terminal__ansi-${color}`}>
                {text}
              </span>
            ))}
            {"\n"}
          </React.Fragment>
        ))}
      </pre>
    </div>
  );
}
