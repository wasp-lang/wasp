import type { WrapperProps } from "@docusaurus/types";
import Tabs from "@theme-original/Tabs";
import type TabsType from "@theme/Tabs";
import clsx from "clsx";
import React, { Children, isValidElement, type ReactNode } from "react";
import styles from "./styles.module.css";

type Props = WrapperProps<typeof TabsType> & { sideBySide?: boolean };

export default function TabsWrapper({
  sideBySide,
  ...props
}: Props): ReactNode {
  if (!sideBySide) {
    return <Tabs {...props} />;
  }

  const tabCount =
    props.values?.length ??
    Children.toArray(props.children).filter(isValidElement).length;

  return (
    <div
      className={clsx(styles.sideBySide)}
      style={{ "--tab-count": tabCount } as React.CSSProperties}
    >
      <Tabs {...props} />
    </div>
  );
}
