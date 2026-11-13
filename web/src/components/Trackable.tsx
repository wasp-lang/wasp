import { type ReactNode } from "react";

import { trackEventAttributes, type DeclaredEvent } from "../lib/analytics";

// Declares the analytics event of its content, for example of a code block in
// Markdown.
export default function Trackable({
  children,
  ...declared
}: DeclaredEvent & { children: ReactNode }): ReactNode {
  return (
    <div {...trackEventAttributes(declared)} className="contents">
      {children}
    </div>
  );
}
