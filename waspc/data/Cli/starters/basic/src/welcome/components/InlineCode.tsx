import { type ReactNode } from "react";

export function InlineCode({ children }: { children: ReactNode }) {
  return <code className="bg-black/5 px-1 py-0.5">{children}</code>;
}
