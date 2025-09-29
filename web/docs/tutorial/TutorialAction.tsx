/*
`TutorialAction` component is related to the Tutorial Actions Executor which you can find in the `web/tutorial-actions-executor` folder.
It has two main purposes:
1. It provides metadata on how to execute tutorial actions programmatically.
2. It renders tutorial action names during development for easier debugging.

`TutorialAction` component is used in the `web/docs/tutorial/*.md` files to annotate specific tutorial actions.
*/

// IMPORTANT: If you change actions here, make sure to also update the types in `web/tutorial-actions-executor/src/actions/actions.ts`.
type ActionProps =
  | {
      action: "INIT_APP";
      starterTemplateName: string;
    }
  | {
      action: "APPLY_PATCH";
    }
  | {
      action: "MIGRATE_DB";
    };

export function TutorialAction({ id, action }: { id: string } & ActionProps) {
  return (
    process.env.NODE_ENV !== "production" && (
      <TutorialActionDebug id={id} action={action} />
    )
  );
}

function TutorialActionDebug({
  id,
  action,
}: {
  id: string;
  action: ActionProps["action"];
}) {
  return (
    <div style={{ display: "flex", gap: "0.5rem", marginBottom: "1rem" }}>
      <div style={tutorialActionPillStyle}>tutorial action: {action}</div>
      <div style={actionPillStyle}>
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

const actionPillStyle: React.CSSProperties = {
  ...pillStyle,
  backgroundColor: "#ef4444",
  display: "flex",
  alignItems: "center",
  gap: "0.25rem",
};
