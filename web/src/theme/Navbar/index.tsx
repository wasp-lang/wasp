import type { WrapperProps } from "@docusaurus/types";
import { trackPlacementAttributes } from "@site/src/lib/analytics";
import Navbar from "@theme-original/Navbar";
import type NavbarType from "@theme/Navbar";
import { type ReactNode } from "react";

type Props = WrapperProps<typeof NavbarType>;

export default function NavbarWrapper(props: Props): ReactNode {
  return (
    <div {...trackPlacementAttributes("header")} className="contents">
      <Navbar {...props} />
    </div>
  );
}
