import { Highlight } from "prism-react-renderer";
import Prism from "prismjs";
import "../css/prism.css";

// Brand-aligned Prism theme — colors set here because prism-react-renderer
// applies them as inline styles, overriding any CSS.
// Background color matches the docs code blocks (see lightCodeTheme in
// docusaurus.config.ts) — keep in sync if either changes.
const waspCodeTheme = {
  plain: {
    color: "#393a34",
    backgroundColor: "#f0ede6",
  },
  styles: [
    {
      types: ["comment", "prolog", "doctype", "cdata"],
      style: { color: "#999", fontStyle: "italic" },
    },
    {
      types: ["string", "attr-value"],
      style: { color: "#777" },
    },
    {
      types: ["punctuation", "operator"],
      style: { color: "#555" },
    },
    {
      types: ["entity", "url", "symbol", "number", "boolean", "variable", "constant", "property", "regex", "inserted"],
      style: { color: "#333" },
    },
    {
      types: ["atrule", "keyword", "attr-name", "tag", "selector"],
      style: { color: "#B8941F", fontWeight: "600" },
    },
    {
      types: ["function", "deleted", "class-name", "builtin", "type", "type-class-name"],
      style: { color: "#333", fontWeight: "600" },
    },
    {
      types: ["important", "bold"],
      style: { fontWeight: "bold" },
    },
    {
      types: ["italic"],
      style: { fontStyle: "italic" },
    },
  ],
};

export default function CodeHighlight({ language, source }) {
  return (
    <Highlight
      Prism={Prism}
      theme={waspCodeTheme}
      code={source}
      language={language}
    >
      {({ className, style, tokens, getLineProps, getTokenProps }) => (
        <pre
          className={className}
          style={{
            borderRadius: 0,
            padding: "16px 0",
            margin: 0,
            height: "100%",
            overflowX: "auto",
            ...style,
          }}
        >
          {tokens.map((line, i) => (
            <div key={i} {...getLineProps({ line })}>
              <span
                style={{
                  display: "inline-block",
                  width: "40px",
                  textAlign: "right",
                  paddingRight: "16px",
                  color: "#BBB",
                  userSelect: "none",
                  fontSize: "0.85em",
                }}
              >
                {i + 1}
              </span>
              {line.map((token, key) => (
                <span key={key} {...getTokenProps({ token })} />
              ))}
            </div>
          ))}
        </pre>
      )}
    </Highlight>
  );
}
