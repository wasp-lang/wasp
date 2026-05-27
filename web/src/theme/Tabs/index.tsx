import type { WrapperProps } from "@docusaurus/types";
import Tabs from "@theme-original/Tabs";
import type TabsType from "@theme/Tabs";
import { type ReactNode } from "react";

type Props = WrapperProps<typeof TabsType>;

export default function TabsWrapper(props: Props): ReactNode {
  return (
    <>
      <Tabs {...props} />
    </>
  );
}
