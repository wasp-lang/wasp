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
  ) : action === "write" ? (
    <div className="relative mb-4 rounded border border-red-500 p-4">
      <div className="absolute right-1 top-1">
        <TutorialActionStep step={step} action={action} />
      </div>
      {children}
    </div>
  ) : (
    <div className="mb-4">
      <TutorialActionStep step={step} action={action} />
    </div>
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
    <div className="flex gap-2">
      <div className="rounded bg-gray-500 px-2 py-1 text-xs font-bold text-white">
        tutorial action: {action}
      </div>
      <div
        className="cursor-pointer rounded bg-red-500 px-2 py-1 text-xs font-bold text-white active:bg-red-600"
        onClick={() => {
          navigator.clipboard.writeText(step);
        }}
      >
        {step}
      </div>
    </div>
  );
}
