import React from "react";

export function TutorialAction({
  children,
  action,
}: React.PropsWithChildren<{
  action: "diff" | "write" | "migrate-db";
}>) {
  if (action === "write") {
    return children;
  }
  return null;
}
