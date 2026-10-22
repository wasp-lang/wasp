import type { WrapperProps } from "@docusaurus/types";
import { trackPlacementAttributes } from "@site/src/lib/analytics";
import MobileSidebar from "@theme-original/Navbar/MobileSidebar";
import type MobileSidebarType from "@theme/Navbar/MobileSidebar";
import { type ReactNode } from "react";

type Props = WrapperProps<typeof MobileSidebarType>;

export default function MobileSidebarWrapper(props: Props): ReactNode {
  return (
    <div {...trackPlacementAttributes("mobile_menu")} className="contents">
      <MobileSidebar {...props} />
    </div>
  );
}
