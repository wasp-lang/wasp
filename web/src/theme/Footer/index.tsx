import type { WrapperProps } from "@docusaurus/types";
import { trackPlacementAttributes } from "@site/src/lib/analytics";
import Footer from "@theme-original/Footer";
import type FooterType from "@theme/Footer";
import { type ReactNode } from "react";

type Props = WrapperProps<typeof FooterType>;

export default function FooterWrapper(props: Props): ReactNode {
  return (
    <div {...trackPlacementAttributes("footer")} className="contents">
      <Footer {...props} />
    </div>
  );
}
