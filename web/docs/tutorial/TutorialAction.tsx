import { useEffect, useRef, useState } from "react";
/*
`TutorialAction` component is related to the Tutorial Actions Executor (TACTE) which you can find in the `web/tutorial-actions-executor` folder.

Its main purpose is to provide metadata on how to execute tutorial actions programmatically (using TACTE)
Additionally, it renders tutorial action names during development for easier debugging.

`TutorialAction` component is used in the `web/docs/tutorial/*.md` files to annotate specific tutorial actions.
*/

// IMPORTANT: If you change actions here, make sure to also update the types in `web/tutorial-actions-executor/src/actions/actions.ts`.
type ActionProps = {
  id: string;
} & (
  | {
      action: "INIT_APP";
      starterTemplateName: string;
    }
  | {
      action: "APPLY_PATCH";
    }
  | {
      action: "MIGRATE_DB";
    }
);

export function TutorialAction(props: React.PropsWithChildren<ActionProps>) {
  const isDevelopment = process.env.NODE_ENV !== "production";

  return isDevelopment ? (
    <TutorialActionDebugInfo {...props} />
  ) : (
    props.children
  );
}

function TutorialActionDebugInfo({
  id,
  action,
  children,
}: React.PropsWithChildren<ActionProps>) {
  const { copied, handleCopy } = useCopy(id);

  return (
    <div style={containerStyle}>
      <div style={headerStyle}>
        <div style={tutorialActionPillStyle}>tutorial action: {action}</div>
        <div style={actionPillStyle}>
          <span style={idTextStyle}>id: {id}</span>
          <button style={copyButtonStyle} onClick={handleCopy}>
            {copied ? "✓ copied" : "copy"}
          </button>
        </div>
      </div>
      {children && <div style={childrenContainerStyle}>{children}</div>}
    </div>
  );
}

function useCopy(textToCopy: string): {
  copied: boolean;
  handleCopy: () => void;
} {
  const [copied, setCopied] = useState(false);
  const timeoutRef = useRef<NodeJS.Timeout | null>(null);

  const handleCopy = () => {
    if (timeoutRef.current) {
      clearTimeout(timeoutRef.current);
    }

    navigator.clipboard.writeText(textToCopy);
    setCopied(true);

    timeoutRef.current = setTimeout(() => {
      setCopied(false);
      timeoutRef.current = null;
    }, 1500);
  };

  useEffect(() => {
    return () => {
      if (timeoutRef.current) {
        clearTimeout(timeoutRef.current);
      }
    };
  }, []);

  return { copied, handleCopy };
}

const containerStyle: React.CSSProperties = {
  marginBottom: "1.5rem",
};

const headerStyle: React.CSSProperties = {
  display: "flex",
  gap: "0.5rem",
  marginBottom: "0.5rem",
  alignItems: "center",
};

const idTextStyle: React.CSSProperties = {
  marginRight: "0.25rem",
};

const copyButtonStyle: React.CSSProperties = {
  fontSize: "0.65rem",
  cursor: "pointer",
  background: "rgba(255, 255, 255, 0.2)",
  border: "1px solid rgba(255, 255, 255, 0.3)",
  borderRadius: "0.25rem",
  padding: "0.125rem 0.375rem",
  color: "white",
  transition: "all 0.2s ease",
};

const childrenContainerStyle: React.CSSProperties = {
  border: "1px dashed #ef4444",
  padding: "1rem",
  borderRadius: "0.5rem",
};

const pillStyle: React.CSSProperties = {
  borderRadius: "0.375rem",
  paddingLeft: "0.625rem",
  paddingRight: "0.625rem",
  paddingTop: "0.375rem",
  paddingBottom: "0.375rem",
  fontSize: "0.75rem",
  fontWeight: "600",
  color: "white",
  boxShadow: "0 1px 2px rgba(0, 0, 0, 0.1)",
};

const tutorialActionPillStyle: React.CSSProperties = {
  ...pillStyle,
  backgroundColor: "#6b7280",
};

const actionPillStyle: React.CSSProperties = {
  ...pillStyle,
  backgroundColor: "#ef4444",
  display: "flex",
  alignItems: "center",
};
