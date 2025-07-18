type Action = "apply-patch" | "migrate-db";

/*
This component serves two purposes:
1. It provides metadata for the `tutorial-app-generator` on how to execute tutorial steps programmatically.
2. It renders tutorial step names during development for easier debugging.
*/
export function TutorialAction({
  action,
  step,
}: {
  action: Action;
  step: string;
}) {
  return (
    process.env.NODE_ENV !== "production" && (
      <div style={{ marginBottom: "1rem" }}>
        <TutorialActionStep step={step} action={action} />
      </div>
    )
  );
}

function TutorialActionStep({
  step,
  action,
}: {
  step: string;
  action: Action;
}) {
  return (
    <div style={{ display: "flex", gap: "0.5rem" }}>
      <div
        style={{
          borderRadius: "0.25rem",
          backgroundColor: "#6b7280",
          paddingLeft: "0.5rem",
          paddingRight: "0.5rem",
          paddingTop: "0.25rem",
          paddingBottom: "0.25rem",
          fontSize: "0.75rem",
          fontWeight: "bold",
          color: "white",
        }}
      >
        tutorial action: {action}
      </div>
      <div
        style={{
          borderRadius: "0.25rem",
          backgroundColor: "#ef4444",
          paddingLeft: "0.5rem",
          paddingRight: "0.5rem",
          paddingTop: "0.25rem",
          paddingBottom: "0.25rem",
          fontSize: "0.75rem",
          fontWeight: "bold",
          color: "white",
          display: "flex",
          alignItems: "center",
          gap: "0.25rem",
        }}
      >
        {step}
        <span
          style={{ fontSize: "0.6rem", cursor: "pointer" }}
          onClick={() => {
            navigator.clipboard.writeText(step);
          }}
        >
          [copy]
        </span>
      </div>
    </div>
  );
}
