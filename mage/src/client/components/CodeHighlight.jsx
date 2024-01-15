import React, { useEffect } from "react";
import Prism from "prismjs";
import "../css/prismjs-github-theme.css";

export function CodeHighlight(props = {}) {
  const codeRef = React.createRef();
  const {
    prefixCls = "code-highlight-wrapper",
    className,
    language,
    source,
    children,
    ...others
  } = props;
  const langCls = language ? `language-${language}` : "";
  function highlight() {
    if (codeRef.current) {
      Prism.highlightElement(codeRef.current);
    }
  }
  useEffect(() => {
    highlight();
  });
  return (
    <pre
      className={`${prefixCls} ${className || ""} ${langCls}`}
      {...others}
      style={{
        borderBottomLeftRadius: "10px",
        borderBottomRightRadius: "10px",
        paddingLeft: "15px",
      }}
    >
      <code className={langCls} ref={codeRef}>
        {source || children}
      </code>
    </pre>
  );
}