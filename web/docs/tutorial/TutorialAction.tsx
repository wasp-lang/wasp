import React from "react";

type Action = "diff" | "write" | "migrate-db";

/*

This component serves two purposes:
1. It provides metadata for the `tutorial-app-generator` on how to execute tutorial steps programmatically.
2. It renders tutorial steps in a visually distinct way during development so it's easier to debug.

*/
export function TutorialAction({
  children,
  action,
  step,
}: React.PropsWithChildren<{
  action: Action;
  step: string;
}>) {
  return process.env.NODE_ENV === "production" ? (
    children
  ) : (
    <div className="mb-4 flex flex-col gap-4 rounded border border-red-500 p-4">
      <div>
        Step <strong>{step}</strong> ({action})
      </div>
      {action === "write" && children}
    </div>
  );
}
