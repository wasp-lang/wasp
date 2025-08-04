type Action = "apply-patch" | "migrate-db";

/*
This component serves two purposes:
1. It provides metadata for the `tutorial-app-generator` on how to execute tutorial steps programmatically.
2. It renders tutorial step names during development for easier debugging.
*/
export function TutorialAction({
  action,
  id,
}: {
  action: Action;
  id: string;
}) {
  return (
    process.env.NODE_ENV !== "production" && (
      <div style={{ marginBottom: "1rem" }}>
        <TutorialActionStep id={id} action={action} />
      </div>
    )
  );
}

function TutorialActionStep({
  id,
  action,
}: {
  id: string;
  action: Action;
}) {
  return (
    <div style={{ display: "flex", gap: "0.5rem" }}>
      <div style={tutorialActionPillStyle}>tutorial action: {action}</div>
      <div style={stepPillStyle}>
        {id}
        <span
          style={{
            fontSize: "0.6rem",
            cursor: "pointer",
          }}
          onClick={() => {
            navigator.clipboard.writeText(id);
          }}
        >
          [copy]
        </span>
      </div>
    </div>
  );
}

const pillStyle: React.CSSProperties = {
  borderRadius: "0.25rem",
  paddingLeft: "0.5rem",
  paddingRight: "0.5rem",
  paddingTop: "0.25rem",
  paddingBottom: "0.25rem",
  fontSize: "0.75rem",
  fontWeight: "bold",
  color: "white",
};

const tutorialActionPillStyle: React.CSSProperties = {
  ...pillStyle,
  backgroundColor: "#6b7280",
};

const stepPillStyle: React.CSSProperties = {
  ...pillStyle,
  backgroundColor: "#ef4444",
  display: "flex",
  alignItems: "center",
  gap: "0.25rem",
};
