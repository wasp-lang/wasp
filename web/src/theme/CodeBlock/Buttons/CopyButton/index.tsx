import type { WrapperProps } from "@docusaurus/types";
import { trackDeclaredCopy } from "@site/src/lib/analytics";
import CopyButton from "@theme-original/CodeBlock/Buttons/CopyButton";
import type CopyButtonType from "@theme/CodeBlock/Buttons/CopyButton";
import { type ReactNode } from "react";

type Props = WrapperProps<typeof CopyButtonType>;

export default function CopyButtonWrapper(props: Props): ReactNode {
  return (
    <span
      className="contents"
      onClickCapture={(event) =>
        trackDeclaredCopy(event.currentTarget, "button")
      }
    >
      <CopyButton {...props} />
    </span>
  );
}
