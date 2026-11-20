import { type ReactNode } from "react";

export function InlineCode({ children }: { children: ReactNode }) {
  return <code>{children}</code>;
}
