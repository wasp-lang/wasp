import React, { useState } from "react";

interface PanelProps {
  /** Filename displayed in the panel title bar */
  title: string;
  /** Short label used for tabs titles */
  tabLabel: string;
  /** A single MDX code block as children */
  children: React.ReactElement;
}

interface CodeSnippetSideBySideProps {
  /** Two Panel elements as children */
  children: [React.ReactElement<PanelProps>, React.ReactElement<PanelProps>];
}

function Panel({ title, children }: PanelProps) {
  return (
    <div className="code-snippet code-snippet--panel">
      <div className="code-snippet__header">
        <div className="code-snippet__dots">
          <span />
          <span />
          <span />
        </div>
        <span className="code-snippet__title">{title}</span>
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

export function CodeSnippetSideBySide({
  children,
}: CodeSnippetSideBySideProps) {
  const [activeTab, setActiveTab] = useState(0);
  const panels = React.Children.toArray(
    children,
  ) as React.ReactElement<PanelProps>[];
  const labels = panels.map((p) => p.props.tabLabel || p.props.title);

  return (
    <div className="code-sbs">
      {/* Tabs */}
      <div className="code-sbs__tabs">
        {labels.map((label, i) => (
          <button
            key={label}
            className={`code-sbs__tab ${i === activeTab ? "code-sbs__tab--active" : ""}`}
            onClick={() => setActiveTab(i)}
          >
            {label}
          </button>
        ))}
      </div>

      {/* Panel content */}
      <div className="code-sbs__grid">
        {panels.map((panel, i) => (
          <div
            key={i}
            className={`code-sbs__cell ${i === activeTab ? "code-sbs__cell--active" : ""}`}
          >
            {panel}
          </div>
        ))}
      </div>
    </div>
  );
}

CodeSnippetSideBySide.Panel = Panel;
