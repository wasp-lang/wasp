import { Highlight, themes } from "prism-react-renderer";
import Prism from "prismjs";
import "../css/prismjs-github-theme.css";

const lightCodeTheme = themes.github;

export default function CodeHighlight({ language, source }) {
  return (
    <Highlight
      Prism={Prism}
      theme={lightCodeTheme}
      code={source}
      language={language}
    >
      {({ className, style, tokens, getLineProps, getTokenProps }) => (
        <pre
          className={className}
          style={{
            borderBottomLeftRadius: "10px",
            borderBottomRightRadius: "10px",
            paddingLeft: "15px",
            ...style,
          }}
        >
          {tokens.map((line, i) => (
            <div key={i} {...getLineProps({ line })}>
              {/* <span>{i + 1}</span> */}
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
