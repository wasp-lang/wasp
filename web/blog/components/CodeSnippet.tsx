import React from "react";

interface CodeSnippetProps {
  /** Filename displayed in the title bar */
  title: string;
  /** A single MDX code block (```lang ... ```) as children */
  children: React.ReactElement;
}

export function CodeSnippet({ title, children }: CodeSnippetProps) {
  return (
    <div className="code-snippet">
      <div className="code-snippet__header">
        <div className="code-snippet__dots">
          <span />
          <span />
          <span />
        </div>
        <span className="code-snippet__title">{title}</span>
        {/* Spacer to keep title centered */}
        <div className="code-snippet__dots" style={{ visibility: "hidden" }}>
          <span />
          <span />
          <span />
        </div>
      </div>
      <div className="code-snippet__body">{children}</div>
    </div>
  );
}
